// table5.hpp

#ifdef OLEDB5BUILD

#define schema5false 0
#define schema5true 1

/*
Class derivations list...


Table5stand::Table5::Table5all::Column5info
                   ::Table5cache::Link5

           ::Table5low::Table5error

Table5server::Table5low::Table5error
            ::Link5

Table5schemaClient::Table5schema
                  ::Table5client::Table5::Table5all::Column5info
                                        ::Table5cache::Link5
                                        ::Table5error

Table5schemaStand::Table5schemaLow
                 ::Table5schemaTag::Table5schema
                                  ::Table5stand::Table5::Table5all::Column5info
                                                       ::Table5cache::Link5
                                                       ::Table5error
                                               ::Table5low

Table5schemaServer::Table5schemaLow
                  ::Table5server::Table5low
                                ::Table5error
                                ::Link5


Table5static::Table5all::Column5info

*/

S5EXPORT COLUMN5INFO * S4FUNCTION d5columns( DATA4 S4PTR *data, short S4PTR *nColumns, int isSchema, Source5config *src, int subtractColumns = 0 ) ;



#ifdef S4SERVER
   class S5CLASS Table5server ;
#endif
class S5CLASS Session5 ;



class Result5update
{
public:
   unsigned short position ;  // The function call position corresponding to the error (Index Origin 0).
   DBROWSTATUS status ; // Use DBROWSTATUS_E_CONCURRENCYVIOLATION to indicate a locking problem.
   ULONG recNo ; // Important information if the record was just appended.
} ;



#ifndef S4CLIENT
   class S5CLASS Table5error
   {
   public:
      void addError( ULONG recNum, DBROWSTATUS status, int pos ) ;
      inline void writeResultPositionIncrement() { if ( errorsList != 0 ) errorsPos++ ; }  // if errorsList NULL, were no errors, so just don't bother

      Table5error() { errorsList = 0 ; errorsMax = 0 ; errorsPos = 0 ; errorsNum = 0 ; }
      ~Table5error() { free5( errorsList ) ; }

      long errorsNum, errorsMax, errorsPos ;
      Result5update *errorsList ;
   } ;
#endif /* not S4CLIENT */



#ifndef S4CLIENT
   class S5CLASS Table5low : public Table5error
   {
   public:
      #ifdef S4SERVER
         Source5all *src ;
         Table5low( Source5all *srcIn, Bool5 isSchemaTableIn )
         {
            src = srcIn ;
            data = 0 ;
            tableName = 0 ;
            isSchemaTableLowVal = isSchemaTableIn ;
            numNullBytesStored = -1 ;
            /*
            tagColumnInfo = 0 ;
            tagColumnInfoTag = 0 ;
            */
         }
         Table5low( Source5all *srcIn, DATA4 *dta, Bool5 isSchemaTableIn )
         {
            src = srcIn ;
            data = dta ;
            tableName = 0 ;
            isSchemaTableLowVal = isSchemaTableIn ;
            numNullBytesStored = -1 ;  // AS 01/21/00 - ensure set to -1 before calling numNullBytesDo()
            numNullBytesDo() ;
            /*
            tagColumnInfo = 0 ;
            tagColumnInfoTag = 0 ;
            */
         }
      #else
         Table5low( Session5 *sesn, Bool5 isSchemaTableIn )
         {
            sess = sesn ;
            data = 0 ;
            tableName = 0 ;
            isSchemaTableLowVal = isSchemaTableIn ;
            numNullBytesStored = -1 ;
         }
      #endif
      ~Table5low()
      {
         closeLow() ;
         free5( tableName ) ;
         tableName = 0 ;
         /*
         if ( tagColumnInfo != 0 )
         {
            free5( tagColumnInfo ) ;
            tagColumnInfo = 0 ;
         }
         */
      }

      void addColumns( int nColumns, FIELD4INFO *fields ) ;
      char *addTag( TAG4INFO *tag ) ;
      inline void closeLow()
      {
         if ( data != 0 )
         {
            #ifdef S4SERVER
               if (isSchemaTable())
                  d4closeTemp( data ) ;
               else
            #endif
               D4close( data ) ;
         }
         data = 0 ;
      }
      virtual void create( const char *name, FIELD4INFO *fields ) ;
      inline int deleted() { return d4deleted( data ) ; }
      void deleteRec( ULONG recNo ) ;
      void flush( Result5update **errorList, long *nError, long count ) ; // Errors correspond to 'write(long, char *)' and 'deleteRec(long)'
      char *generateTagName() ;
      #ifdef S4SERVER
         int go( ULONG recNo, int readOnly = 0, Table5rowsetValues *rowsetValues = 0, int ensureFresh = 0 ) ;
         virtual inline COLUMN5INFO *getColumns( short *nColumns, int *freeRequired )
         {
            COLUMN5INFO *outInfo = d5columns( data, nColumns, 0, src ) ;
            if ( outInfo != 0 )
               *freeRequired = 1 ;
            return outInfo ;
         }
      #else
         virtual int go( ULONG recNo, int readOnly = 0, Table5rowsetValues *rowsetValues = 0, int ensureFresh = 0 ) ;
         virtual inline COLUMN5INFO *getColumns( short *nColumns, int *freeRequired )
         {
            COLUMN5INFO *outInfo = d5columns( data, nColumns, 0, sess->source ) ;
            if ( outInfo != 0 )
               *freeRequired = 1 ;
            return outInfo ;
         }
      #endif
      int isFieldDeferred( int iOrdinal ) ;
      int lockIsolationRead( ULONG recNo ) ;  // read lock based on isolation level before read
      void lockIsolationReadDone( ULONG recNo ) ; // read lock based on isolation level after read
      inline int lockRead( ULONG recNo ) { return d4lockInternal( data, recNo, 1, lock4read ) ; }    // perform a read lock on a record
      inline int lockReadFile() { return lockWriteFile() ; } // perform a read lock on a file
//      COLUMN5INFO *getSelectedTagColumnInfo( short *nColumns ) ;
      virtual inline const char *name()
      {
         // AS 05/12/99 --> need to trim the ".dbf" extension...
         if ( tableName != 0 )
            return tableName ;
         int len = strlen( data->dataFile->file.name ) ;  // includes extension always
         tableName = (char *)alloc5( len + 1 ) ;
         memcpy( tableName, data->dataFile->file.name, len ) ;
         tableName[len] = 0 ;
         #if !defined(S4CLIENT) && !defined(S4UNIX)
            // AS 02/21/00 unix change, must upper case name...
            strupr( tableName ) ;
         #endif
         #ifndef S5USE_DBF_EXTENSION_IN_SCHEMAS
            u4nameRemoveGivenExtension( tableName, "DBF" ) ;
         #endif
         return tableName ;
      }
      inline short numNullBytes() { assert5( numNullBytesStored != -1 ) ; return numNullBytesStored ; }
      // #ifdef S4STAND_ALONE
      //    void open( char *name ) ;  // low-level open
      // #endif
      inline ULONG recCount() { return D4recCount( data ) ; }
      inline int recCountGreater( ULONG cmp ) { return d4recCountGreater( data, cmp ) ; }
      inline int recCountLess( ULONG cmp ) { return d4recCountLess( data, cmp ) ; }
      inline int recCountLessEq( ULONG cmp ) { return d4recCountLessEq( data, cmp ) ; }
      inline ULONG recNo() { return d4recNo( data ) ; }
      void remove() ;
      void removeIndexes() ;
      #ifdef S4SERVER
         void rowWrite( ULONG recNo, ByRef5 *deferredFields, Fieldset5server *fieldset ) ; // writes a row; frees memory referenced by 'deferredFields'
      #else
         void rowWrite( ULONG recNo, ByRef5 *deferredFields, Fieldset5all *fieldset ) ; // writes a row; frees memory referenced by 'deferredFields'
      #endif
      char tagExists( char *tagName ) ;
      inline void unlockRead( ULONG recNo ) { unlockWrite( recNo ) ; }  // perform a read unlock on a record
      char *writeGetRawBuffer( ULONG recNo, char areAllFieldsBound ) ;
      void removeColumn( char *columnName ) ;
      virtual inline WSTR5 *getName( int ordinal ) = 0 ;  // must be overridden by a higher-level function...
      virtual inline unsigned short numRegisteredFields() { return numFields() ; }  // # of fields with field info from col info and in schema rowset

      #ifdef S4SERVER
         inline Session5 *session() ;
      #else
         inline Session5 *session()
         {
            return sess ;
         }
      #endif

      inline Source5all *source()
      {
         #ifdef S4STAND_ALONE
            return sess->source ;
         #else
            return src ;
         #endif
      }

      DATA4 *data ;    // actual DATA4 which is used to perform the table operations
      inline Bool5 isSchemaTable() { assert5( isSchemaTableLowVal != -1 ) ; return isSchemaTableLowVal ; } // needs to be overriden
      int tagSelect( char *tagName, int *iRowset ) ;  // returns 0 if ok, < 0 if error (no tag)
      TAG4 *tag( char *tagName, int *iRowset ) ;  // returns 0 if ok, < 0 if error (no tag)
   protected:
      // takes into account ordinal mismatching due to ole-db if reqd.
      inline const char *fieldName( int ordinal ) { return f4name( d4fieldJ( data, ordinal ) ) ; }
      // write lock based on isolation level before write
      inline int lockIsolationWrite( ULONG recNo ) { return lockWrite( recNo ) ;}
      // write lock based on isolation level after write
      inline void lockIsolationWriteDone( ULONG recNo )
      {
         if ( session()->isolationLevel() == NO5TRANSACTION )
            unlockWrite( recNo ) ;
      }
      inline int lockWrite( ULONG recNo )  // perform a write lock on a record
      {
         // recNo 0 means append
         return ( ( (recNo) == 0 ) ? d4lockAppendRecord( data, 1 ) : d4lockInternal( data, (recNo), 1 ) ) ;
      }
      inline int lockWriteFile() { return d4lockFileInternal( data, 1 ) ; }   // perform a write lock on a file

      #ifdef S4INCLUDE_RECNO_DELETED
         // the fields include the recno and deleted fields...
         virtual inline unsigned short numFields() { return d4numFields( data ) + 2 ; }  // 1 for deleted and 1 for recno
      #else
         // the fields DO NOT include the recno and deleted fields...
         virtual inline unsigned short numFields() { return d4numFields( data ) ; }
      #endif
      #ifdef S4STAND_ALONE
         void openLow( const char *name ) ;
      #endif
      /*
      inline Table5error *Table5errorPtr( Table5low *t5low )
      {
         assert5( t5low != 0 ) ;

         #ifdef S4SERVER
            return (Table5error *)((Table5server *)t5low) ;
         #endif
         #ifdef S4STAND_ALONE
            return (Table5error *)((Table5stand *)t5low) ;
         #endif
      }
      */

      void unlockWrite( ULONG recNo ) ;

      #ifndef S4SERVER
         Session5 *sess ;
      #endif
      void numNullBytesDo() ;
   private:
      short numNullBytesStored ;
      inline int nobodyElseCanWrite()
      {
         // don't include the file lock case because someone may have got to it before our lock
         return ( data->dataFile->file.lowAccessMode != OPEN4DENY_NONE ) ;
      }

      char *tableName ;
      Bool5 isSchemaTableLowVal ;
//      COLUMN5INFO *tagColumnInfo ;
//      short tagColumnNumColumns ;
//      TAG4 *tagColumnInfoTag ;
   } ;
#endif /* not S4CLIENT */



#ifndef S4SERVER
   class S5CLASS Table5all : public Column5info // Base class of table and index classes
   {
   public:
      Table5all( Bool5 isSchemaTableIn ) { version = 0 ; isSchemaTableVal = isSchemaTableIn ; }

      inline virtual void close() = 0 ;
      inline Bool5 isSchemaTable() { return isSchemaTableVal ; }
      virtual void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) = 0 ; // , int *offsetRaw = 0 ) = 0 ;
                  // If it is a bad ordinal, does "throw DBBINDSTATUS_BADORDINAL"
      inline int getFieldInfoLen( int ordinal )
      {
         #ifdef S4CLIENT
            assert5( ordinal <= nColumns() && ordinal >= 0 ) ;
            #ifdef S4INCLUDE_RECNO_DELETED
               return( ( ordinal == 0 || ordinal - 2*isSchemaTable() == nColumns() - 1 ) ? 4 :
                       ( ( columns()[ordinal - isSchemaTable()].isDeferred ) ? 0 : columns()[ordinal - isSchemaTable()].rawLen ) ) ;
            #else
               return( ( ordinal == 0 ) ? 4 :
                       ( ( columns()[ordinal - isSchemaTable()].isDeferred ) ? 0 : columns()[ordinal - isSchemaTable()].rawLen ) ) ;
            #endif
         #else
            int len ;
            getFieldInfo( ordinal, &len, 0, 0, 0 ) ; // , 0 ) ;
            return len ;
         #endif
      }
      Type5 getFieldInfoRawType( WSTR5 *columnName ) throw( Err5internal ) ;
      virtual inline short keyLen() { throw Err5internal( 0 ) ; }   // only gets called for table objects which override this, else error (i.e. schemas and indexes)
      virtual int isFieldDeferred( int iOrdinal ) = 0 ;
      virtual HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) = 0 ;
      virtual unsigned short numFields() = 0 ;
      virtual short numNullBytes() = 0 ;
      virtual void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, int iRowset, char countDeletedAsSkip = 0 ) = 0 ;
      virtual int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) = 0 ;
      virtual void rowRequestSkipNext( Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent ) { throw Err5internal(0) ; }  // must implement if implementing rowRequestGetNext for sequential rowsets
      virtual void rowRequestGetDeferredFields( Fieldset5all *fields, ULONG recNo, char **rawBuffer, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues, int iRowset ) = 0 ;
//      virtual void updateFieldset( Fieldset5all *fieldset, Table5rowsetValues *rowsetValues ) = 0 ; // Only updates if necessary; not necessary to do anything in stand-alone table case.
      virtual void updateFieldset( Fieldset5all *fieldset, int *iRowset ) = 0 ; // Only updates if necessary; not necessary to do anything in stand-alone table case.
      virtual int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) = 0 ;
      virtual void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) = 0 ;
      void updateRawOffsets( Fieldset5all *fieldSet ) ;
      virtual int numDeferredFields() = 0 ;
      virtual int isReadOnly( int iOrdinal ) = 0 ;
      virtual ULONG maxCount() = 0 ;
      virtual inline WSTR5 *getName( int ordinal )   // overriden by schema for long-name support
      {
         #ifdef E4DEBUG
            // verify that we are not accessing outside of array...
            short nCols ;
            columns( &nCols ) ;
            assert5( ordinal < nCols ) ;
         #endif
         return (columns( 0 ))[ordinal].name ;
      }
      virtual COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) = 0 ;
      long version ;  // used in some instances to check whether certain tables types (namely schema) have been changed
      #ifdef S4CLIENT
         virtual short iDataGet() = 0 ;
      #else
         virtual inline long seek( const char *key, short len, int descend = 0 ) { throw Err5internal(0) ; return -1 ; }  // only schema tables implement
      #endif
      virtual inline int isStaticTable() { return 0 ; }  // only static tables override to return '1'
      virtual inline Collate4name collateName() { return collate4none ; }  // used for collated index rows only, by default not known
      // AS 08/19/99 -- added, loss of info on some index types (eg. general collation)
      virtual inline Bool5 lossOfInfo( int iOrdinal )
      {
         assert5( columns()[iOrdinal - isSchemaTable()].ordinal == iOrdinal ) ;
         assert5( columns()[iOrdinal - isSchemaTable()].lossOfInfo == 0 || columns()[iOrdinal - isSchemaTable()].lossOfInfo == 1 ) ;
         return ( columns()[iOrdinal - isSchemaTable()].lossOfInfo ) ;
      }
   private:
      Bool5 isSchemaTableVal ;
   } ;
#endif /* not S4SERVER */



#ifndef S4SERVER
   class S5CLASS Table5cache : public Link5   // on list of open and cached for Session5
   {
      // This class is used to cache open data files for the Session5.
      // the main purpose of this class is to restrict each Session to disallow cloned DATA4's
      // we would prefer to have multiple handles to the same DATA4 because otherwise we run
      // into transactional problems (namely different DATA4's cannot share locks).
   public:
      virtual ~Table5cache() { ; }
      void release() ;   // reduces the refCount by one, if zero, call cached close
      inline void addOpenRef()  { openRefCount++ ; } // increase the refCount by one
      virtual const char *cacheName() = 0 ;
      int type ;  // schema, stand-alone, client, server, etc.
      inline void remove() { isDropped = 1 ; }
      #ifdef S4CLIENT
         virtual short iDataGetLow() = 0 ;
      #endif
      #ifdef S4STAND_ALONE
         virtual DATA4 *data4() = 0 ;
         inline void versionCheck() { d4versionCheck( data4() ) ; }
         inline void freeBlocks() { d4freeBlocks( data4() ) ; }
         // virtual Table5stand *getTableStandPtr() = 0 ;
      #endif
   protected:
      Session5 *sessionCache ;   // distinguish from Table5low::session
      int openRefCount ;   // when it hits zero, we can close the data file
      char isDropped ;

      Table5cache( Session5 *sesn, int tType ) { sessionCache = sesn ; openRefCount = 0 ; type = tType ; isDropped = 0 ; }
   } ;
#endif /* not S4SERVER */



#ifndef S4SERVER
   class S5CLASS Table5 : public Table5cache, public Table5all
   {
      // This class is used to define the virtual functions which a Table5client and
      // Table5stand must implement in order to provide support for a RowSet5.
      // By creating this class, the RowSet5 class can be implemented without using
      // #ifdefs to different structures depending on whether it is implemented as client
      // or stand-alone
   public:
      Table5( Session5 *session, int tType, Bool5 isSchemaTableIn ) : Table5cache( session, tType), Table5all( isSchemaTableIn ) { ; }

      inline virtual void close() { Table5cache::release() ; }
      virtual void addColumns( int nColumns, FIELD4INFO *fields ) = 0 ;
      virtual char * addTag( TAG4INFO *tag ) = 0 ;
      virtual void create( const char *name, FIELD4INFO *fields ) = 0 ;
      virtual void deleteRec( ULONG recNo ) = 0 ;
      virtual void flush( Result5update **errorList, long *nError, long count ) = 0 ; // Errors correspond to 'write(ULONG, char *)' and 'deleteRec(long)'
      virtual const char *name() = 0 ;
      virtual void openLow( const char *name ) = 0 ;
      virtual void remove( void ) = 0 ;
      virtual void removeColumn( char *columnName ) = 0 ;
      virtual void removeIndexes() = 0 ;
      virtual void rowRequestArray( ULONG **recNoArray, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset ) = 0 ;
      virtual void write( ULONG recNo, ByRef5 *byRefBuffer, Fieldset5all *fieldSet, int iRowset ) = 0 ;
//      virtual void write( ULONG recNo, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues ) = 0 ; // Uses the buffer returned last by writeGetRawBuffer()
//      virtual char *writeGetRawBuffer( ULONG recNo, Table5rowsetValues *rowsetValues, char areAllFieldsBound, ULONG lenFieldsRaw ) = 0 ; // Must be called just before 'write(ULONG,char *)'
      virtual void defaultRawValues( char **rawBuffer, ByRef5 **byRefBuffer, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, int iRowset ) = 0 ;
      virtual inline void rowRequestSkipNext( Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent ) ;
      virtual inline Schema5type schemaType() ;
      virtual void writeResultPositionIncrement() = 0 ;
      virtual ULONG recCount() = 0 ;
   //   inline ULONG maxCount() { return recCount() ; }  // for data files, max count == rec count
      inline ULONG maxCount() { return ROWSET5MAX_GET_HANDLES ; }  // for data files, max count == rec count
      virtual Bool5 hasIndexAvailable() = 0 ;  // is an index tag available from the table? - does an index exist?
      virtual inline int tagSelect( char *tagName, int *iRowset ) { return -1 ;  }  // only overrided where required (i.e. not for schemas)
      virtual inline TAG4 *tag( char *tagName, int *iRowset ) { return 0 ;  }  // only overrided where required (i.e. not for schemas)
      #ifndef S4CLIENT
         virtual DATA4 *data4() = 0 ;
         virtual Table5low *tableLow() = 0 ;
         virtual ULONG skip( ULONG start, LONG numSkip, TAG4 *tagSelected ) = 0 ;
      #endif
      virtual char *generateTagName() ;
      long getColumnIdFromWidestringName( LPCWSTR fName ) ;
      // virtual Table5stand *getTableStandPtr() { throw Err5internal( 0 ) ; return 0 ; }  // should be derived if used...

//      virtual COLUMN5INFO *getSelectedTagColumnInfo( short *nColumns ) { throw Err5internal(sessionCache->c4) ; }  // should be overriden for classes that need this (i.e. NOT schema)
      virtual unsigned short numFields() = 0 ;
      #ifdef S4INCLUDE_RECNO_DELETED
         inline int isReadOnly( int iOrdinal ) { if ( iOrdinal == 0 || iOrdinal > (numFields()-2) ) return 1 ; return 0 ; }
      #else
         inline int isReadOnly( int iOrdinal ) { if ( iOrdinal == 0 ) return 1 ; return 0 ; }
      #endif

      virtual inline HRESULT find( Accessor5all *a5, unsigned long *startRowNo, const void *seekKey, int seekValueLen,
           int skipAmount, char doConversion, DBCOMPAREOP CompareOp, int fieldOrdinal, int direction ) ;
   } ;
#endif /* not S4SERVER */



#ifdef S4SERVER
   class S5CLASS Table5serverBase
   {
   public:
      void init( Table5server *tbl ) { table = tbl ; }  // fieldSet.init( tbl ) ; }
      void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
      void operator delete( void *p ) { free5(p) ; }

      Table5server *table ;

      inline int goRead( ULONG recNo, Table5rowsetValues *rowsetValues = 0 )
      {
          return go( recNo, 1, rowsetValues, 0 ) ;
      }
      virtual int go( ULONG recNo, int readOnly = 0, Table5rowsetValues *rowsetValues = 0, int ensureFresh = 0 ) ;
      virtual void tagSelect( char *tagName, int *iRowset ) ;
      virtual long seek( const char *key, short len, int descend = 0 ) ;
      virtual ULONG skip( ULONG start, LONG numSkip, TAG4 *tagSelected ) ;
      virtual DATA4 *data4() ;
      virtual COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) ;
      int clientsVersion ;   // used for schema tables for if changed...
   } ;
#endif /* S4SERVER */



#ifdef S4SERVER
   class S5CLASS Table5server : public Link5, public Table5low, public Column5info
   {
      // uses the Table5low base class to perfrom low-level operations, then returns the
      // results to clients
   public:

      void * operator new( size_t s ) { void *ptr = alloc5(s) ; return ptr ; }
      void operator delete( void *p ) { free5(p) ; }

      // this next constructor is for schema tables only - or for when creating a table
      Table5server( Source5all *src, Bool5 isSchemaTableIn ) : Table5low( src, isSchemaTableIn ) {}

      // now the normal constructor
      Table5server( Source5all *src, Bool5 isSchemaTableIn, DATA4 *data ) ;
      void tagSelect( char *tagName, int *iRowset ) ;  // used for schemas
      long seek( const char *key, short len, int descend = 0 ) ;  // used for schemas
      inline DATA4 *data4() { return data4local() ; }
      void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) ; //, int *offsetRaw = 0 ) ;
      inline int getFieldInfoOffset( int ordinal )
      {
         // server implementation of columns - for non-schema, the 0th entry is the recno field, so
         // just access using the ordinal directly (ordinal = array entry)
         return( columns()[ordinal - isSchemaTable()].offsetRaw ) ;
      }
      inline int getFieldInfoLen( int ordinal )
      {
         return( columns()[ordinal - isSchemaTable()].rawLen ) ;
      }
      virtual inline COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) { return Table5low::getColumns( nColumns, freeRequired ) ; }
      virtual inline WSTR5 *getName( int ordinal ) { return (columns())[ordinal].name ; }   // overriden by schema for long-name support
      virtual inline void create( const char *name, FIELD4INFO *fields ) ;
//      virtual inline int isSchemaTable() { return 0 ; }  // schema tables override
      int table5schemaTagIsDescending() ;
      inline DATA4 *data4local() { return data ; }  // use local function where possible because non-virtual is much faster
   } ;
#endif /* S4SERVER */



#ifdef S4CLIENT
   class S5CLASS Table5client : public Table5
   {
      // Class with functions which use the communications module to request and retrieve information
      // from the CodeServer
   public:
      Table5client( Session5 *sesn, Bool5 isSchemaTableIn ) ;
      virtual ~Table5client() ;   // tell server to release its version

      void addColumns( int nColumns, FIELD4INFO *fields ) ;
      char * addTag( TAG4INFO *tag ) ;
      inline const char *cacheName() { return tName ; }
      inline virtual void close() { Table5cache::release() ; }
      virtual void create( const char * name, FIELD4INFO *fields ) ;
      void defaultRawValues( char **rawBuffer, ByRef5 **byRefBuffer, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, int iRowset ) ;
      void deleteRec( ULONG recNo ) ;
      ULONG fetchNextRecNo( ULONG startRecNo, int increment, Table5rowsetValues *rowsetValues, int iRowset ) ;
      void flush( Result5update **errorList, long *nError, long count ) ; // Errors correspond to 'write(long, char *)' and 'deleteRec(long)'
      COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) ;
      void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) ; //, int *offsetRaw = 0 ) ;
      inline short iDataGet() { return iData ; }
      inline short iDataGetLow() { return iData ; }
      Bool5 hasIndexAvailable() { throw Err5internal( 0 ) ; } // not coded yet
      int isFieldDeferred( int iOrdinal ) ;
      HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) ;
      virtual inline const char *name() { return tName ; }
      inline int numDeferredFields() { throw Err5client() ; return -1 ;}  // not coded yet... but need to track this somewhere...
      // AS 01/19/01 - Was returning nFields+2 instead of just nFields.  why?  makes no sense, was causing problems
      // virtual inline unsigned short numFields() { return (int)nFields + 2 ; }
      virtual inline unsigned short numFields() { return (int)nFields ; }
      inline short numNullBytes() { return numberNullBytes ; }
      void openLow( const char *name ) ;   // tell server to set itself up, call Table5cache::open()
      int recCountLessThan( unsigned long val ) ;
      ULONG recCount() ;
   //   void registerFieldSet( Fieldset5all *fieldSet ) ;  // new field set info for server
      void remove() ;
      void removeColumn( char *columnName ) ;
      void removeIndexes() ;
      void rowRequestArray( ULONG **recNoArray, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset ) ;
      void rowRequestDeferredField( int fieldOrdinal ) ;
      void rowRequestGetDeferredFields( Fieldset5all *fields, ULONG recNo, char **rawBuffer, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues, int iRowset ) ;
      int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) ;
      void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, int iRowset, char countDeletedAsSkip = 0 ) ;
      void rowRequestSkipNext( Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent ) ;
      void updateFieldset( Fieldset5all *fieldset, int *iRowset ) ; // Only updates if necessary
      // update offsets in client must take into account duplicate fields, therefore duplicate offsets
      int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) ;
      void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) ;
      void updateVersions() ;
//      void write( ULONG recNo, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues ) ;
      void write( ULONG recNo, ByRef5 *byRefBuffer, Fieldset5all *fieldSet, int iRowset ) ;
      char *writeGetRawBuffer( ULONG recNo, Table5rowsetValues *rowsetValues, char areAllFieldsBound, ULONG lenFieldsRaw ) ;
      void writeResultPositionIncrement() ;
      int tagSelect( char *tagName, int *iRowset ) ;  // returns 0 if ok, < 0 if error (no tag)
      // AS 05/25/99 - TAG4 not avail in client...
      TAG4 *tag( char *tagName, int *iRowset ) { throw Err5internal( 0 ) ; return 0 ; }

      short         iData ;     // index into server's Table5server array
      long          nFields ;
      Session5     *session ;
      char         *tName ;     // when create/openLow, save name for return by name() function
      unsigned long minRecCount ;
   protected:
      CODE4        *c4 ;
      short         numberNullBytes ;
      char         *retrieveRawBuffer ;
      unsigned long retrieveRawBufferLen ;
   private:
      ByRef5       *byRefBufferDefault ;
      ULONG         byRefBufferDefaultCount ;
      Ptr5lenAlloc *errorList ;
      ULONG         nullBytesOffset ;
      char         *rawBufferDefault ;
      ULONG         rawBufferDefaultLen ;
      char         *writeBuffer ;
      ULONG         writeBufferLen ;
   } ;
#endif /* S4CLIENT */



#ifdef S4STAND_ALONE
   class S5CLASS Table5stand : public Table5, public Table5low
   {
      // Class interacts with base class Table5low to perform low-level operations, uses
      // base class Table5cache to cache the opens.
   public:
      Table5stand( Session5 *sesn, Bool5 isSchemaTableIn, int tType = TYPE5STAND ) ;
      virtual ~Table5stand() ;

      inline Table5low *tableLow() { return this ; }

      inline DATA4 *data4() { return data4local() ; }  // don't make virtual - don't derive from
      virtual inline unsigned short numFields() { return Table5low::numFields() ; }
      inline void addColumns( int nColumns, FIELD4INFO *fields ) { Table5low::addColumns( nColumns, fields ) ; }
      virtual inline const char *cacheName() { return Table5low::name() ; }
      virtual inline COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) { return Table5low::getColumns( nColumns, freeRequired ) ; }
      inline virtual void create( const char *name, FIELD4INFO *fields ) { Table5low::create( name, fields ) ; Table5cache::addOpenRef() ; }
      inline void deleteRec( ULONG recNo ) { Table5low::deleteRec( recNo ) ; }
      inline void flush( Result5update **errorList, long *nError, long count ) // Errors correspond to 'write(ULONG, char *)' and 'deleteRec(long)'
         { Table5low::flush( errorList, nError, count ) ; }
      virtual void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) ; //, int *offsetRaw = 0 ) ;
      inline int isFieldDeferred( int iOrdinal ) { return Table5low::isFieldDeferred( iOrdinal ) ; }
      inline Bool5 isSchemaTable() { return Table5low::isSchemaTable() ; }
      HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) ;
      virtual inline const char *name() { return Table5low::name() ; }
      inline void openLow( const char *name ) { Table5low::openLow( name ) ; }
      inline ULONG recCount() { return Table5low::recCount() ; }
      inline void remove( void ) ;
      inline void removeColumn( char *columnName ) { Table5low::removeColumn( columnName ) ; }
      inline void removeIndexes() { Table5low::removeIndexes() ; }
      void rowRequestArray( ULONG **recNoArray, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset ) ;
      void rowRequestGetDeferredFields( Fieldset5all *fields, ULONG recNo, char **rawBuffer, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues, int iRowset ) ;
      int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) ;
      void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, int iRowset, char countDeletedAsSkip = 0 ) ;
      virtual Bool5 hasIndexAvailable() { return ( d4tagDefault( data4() ) != 0 ) ; }  // is an index tag available from the table? - does an index exist?
      // Table5stand *getTableStandPtr() { return this ; }
      inline void updateFieldset( Fieldset5all *fieldsetParm, int *iRowset ) // Only updates if necessary
         { ; }
//         { rowsetValues->fieldsetCurrent = fieldsetParm ; }
      int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) ;
      void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) ;
      inline void write( ULONG recNo, ByRef5 *byRefBuffer, Fieldset5all *fieldSet, int iRowset )
         {
            // Uses the buffer returned last by writeGetRawBuffer()
            rowWrite( recNo, byRefBuffer, fieldSet ) ;
         }
      inline int goRead( ULONG recNo, Table5rowsetValues *rowsetValues )
      {
         return go( recNo, 1, rowsetValues, 0 ) ;
      }
      inline int goWrite( ULONG recNo, Table5rowsetValues *rowsetValues )
      {
         return go( recNo, 0, rowsetValues ) ;
      }
//      COLUMN5INFO *getSelectedTagColumnInfo( short *nColumns ) { return Table5low::getSelectedTagColumnInfo( nColumns ) ; }
      inline void rowWrite( ULONG recNo, ByRef5 *deferredFields, Fieldset5all *fieldset ) { Table5low::rowWrite( recNo, deferredFields, fieldset ) ; }
//      inline char *writeGetRawBuffer( ULONG recNo, Table5rowsetValues *rowsetValues, char areAllFieldsBound, ULONG lenFieldsRaw )
//         { return Table5low::writeGetRawBuffer( recNo, rowsetValues, areAllFieldsBound, lenFieldsRaw ) ; }
      void defaultRawValues( char **rawBuffer, ByRef5 **byRefBuffer, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, int iRowset ) ;
      long getFieldInfoOffset( int ordinal ) ;
      long maxDeferredFields() ;
      HRESULT find( Accessor5all *a5, unsigned long *startRowNo, const void *seekKey, int seekValueLen, int skipAmount, char doConversion, DBCOMPAREOP CompareOp, int fieldOrdinal, int direction ) ;
      inline int numDeferredFields() { return Table5low::data->dataFile->nFieldsMemo ; }  // for stand-alone, memo fields only are deferred
      void rowRequestSkipNext( Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent ) ;
      inline short numNullBytes() { return Table5low::numNullBytes() ; }
      inline char *addTag( TAG4INFO *tag ) { return Table5low::addTag( tag ) ; }
      inline char *generateTagName() { return Table5low::generateTagName() ; }
      inline void writeResultPositionIncrement() { Table5low::writeResultPositionIncrement() ; }
      inline DATA4 *data4local() { return data ; }  // use local function where possible because non-virtual is much faster
      virtual inline WSTR5 *getName( int ordinal ) { return Table5all::getName( ordinal ) ; }
//      virtual inline int isSchemaTable() { return 0 ; }
      inline int tagSelect( char *tagName, int *iRowset ) { return Table5low::tagSelect( tagName, iRowset ) ;  }  // only overrided where required (i.e. not for schemas)
      inline TAG4 *tag( char *tagName, int *iRowset ) { return Table5low::tag( tagName, iRowset ) ;  }  // only overrided where required (i.e. not for schemas)
      ULONG skip( ULONG start, LONG numSkip, TAG4 *tagSelected ) ;

      // AS Feb 20/02 - customized work to select a default index if not provided...
      const char *getDefaultIndex() ;
   private:
      ULONG rowRequestPositionViaOffset( ULONG startRecNo, LONG offset, Table5rowsetValues *rowsetValues, char countDeletedAsSkip ) ;
      ULONG rowRequestRecordCheck( ULONG startRecNo, const LONG offset, const LONG cRecNo, Table5rowsetValues *rowsetValues, const char setOldDirection ) ;
      ULONG rowRequestStartFromValidPosition( ULONG startRecNo, const LONG offset, const LONG cRecNo, Table5rowsetValues *rowsetValues, const char setOldDirection ) ;
      ULONG rowRequestDetermineStartFromUnknownState( ULONG startRecNo, const LONG offset, const LONG cRecNo, int *doRecCheck, Table5rowsetValues *rowsetValues ) ;
      ULONG rowRequestResetFromEndpoint( ULONG startRecNo, Table5rowsetValues *rowsetValues ) ;
      void rowRequestSequentialVerifyStartWithOffset( const ULONG startRecNo, const LONG offset, const LONG cRecNo, Table5rowsetValues *rowsetValues ) ;
      ULONG bottomRecno( TAG4 *tagSelected ) ;
      ULONG topRecno( TAG4 *tagSelected ) ;
      void position( ULONG rec, TAG4 *tagSelected ) ;
      inline int goReadEnsureLatest( ULONG recNo, Table5rowsetValues *rowsetValues = 0 )
      {
         { return go( recNo, 1, rowsetValues, 1 ) ; }
      }
   //   Fieldset5all *fieldset ;
      char *rawBufferDefault ;
      ByRef5 *byRefBufferDefault ;
   } ;
#endif /* S4STAND_ALONE */



#ifndef S4SERVER
   class S5CLASS Table5schema
   {
      // class to perform table operations on a schema (as opposed to
      // optimization operations).  also contains storage for the current position
   public:
      Table5schema() { currentKey = 0 ; currentRecno = 0L ; currentLen = 0 ; }
      ~Table5schema() ;

      #ifndef S4CLIENT
         /* client uses the same as Table5client one */
         virtual int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) = 0 ;
         virtual long seek( const char *key, short len, int descend = 0 ) = 0 ;
      #endif
      virtual void rowRequestSchemaSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset ) = 0 ;

      virtual WSTR5 *getName( int ordinal ) = 0 ;  // for long-name support, must override
      char *currentKey ;
      short currentLen ;
      ULONG currentRecno ;
   } ;
#endif /* S4SERVER */



#ifdef S4CLIENT
   class S5CLASS Table5schemaClient : public Table5schema, public Table5client
   {
   public:
      Table5schemaClient( Session5 *sessn, Schema5type schId ) : Table5client( sessn, 1 ) { schemaTypeId = schId ; }

//      virtual inline int isSchemaTable() { return 1 ; }  // schema tables override
      void openLow( const char *name ) ;
      ULONG rowNumberRetrieve() ; // read long off of communications stream ;
      inline int rowPosition( void )  { return r4success ; }   // client does not implement this function
      void rowRequestSchemaSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset ) ;
      virtual inline Schema5type schemaType() { return schemaTypeId ; }
      inline int tagLock() { return 0 ; }   // client does not worry about locking the tag --> server takes care of it
      inline void tagUnlock() { ; }   // client does not worry about unlocking the tag --> server takes care of it
      virtual inline WSTR5 *getName( int ordinal )  // overriden by schema for long-name support
      {
         //assert5( cols != 0 ) ;
         return columns()[ordinal].name ;
      }

      char *rawRetrieve( ULONG recNo, Fieldset5all *fieldsetCurrent, short iRowset ) ;
   //   int open() ;
   //   void rowRequestSchema( LONG cRowsOffset, LONG cRecNo ) ;
   //   long seek( const char *key, short len, int descend = 0  ) ;  // returns recno
   //   setNextPosition() ; // { currentKey = read off comm. stream, currentRecno = read of comm. stream ; }
   //   virtual WSTR5 *getName( int ordinal ) = 0 ;  // for long-name support, must override
   private:
      Schema5type schemaTypeId ;
   } ;
#endif /* S4CLIENT */



#ifdef S4STAND_ALONE
   class S5CLASS Table5schemaTag : public Table5schema, public Table5stand
   {
      // contains the actual tag operations which are performed on the current tag for the
      // schema, which represents the positioning for the schema
      // this class builds on the Table5schema which contains storage for the current position
      // along with the basic data functionality which must be supported for the Schema
   public:
      Table5schemaTag( Session5 *sessn ) : Table5stand( sessn, TYPE5SCHEMA ) { ; }

      inline char *key() { return tfile4key( tagFile() ) ; }
      inline short keyLen() { return I4MAX_KEY_SIZE ; }  // key sizes vary, so to be safe set at maximum possible
      // position to last record which matches the restrictions
      inline void positionTagBottom() { Table5schemaTag::seek( currentKey, currentLen, 1 ) ; }
      int positionTagCurrent() ;   // positiositionTagCurrent() ;   // position based on currentKey, currentRecNo
      void positionTagTop() ;  // position to last record which matches the restrictions
      inline ULONG recNo() ;
      inline virtual Schema5type schemaType() ;
      int skipNoKeyChange( LONG cRowsOffset ) ;
      int table5schemaTagIsDescending() ;
      virtual WSTR5 *getName( int ordinal ) = 0 ;  // for long-name support, must override
      inline int tagLock()
      {
         if(s5clipper)
            return tfile4lock( tagFile(), data4serverId( data4()) ) ;
         else
            return d4lockIndex( data4() ) ;
      }
      inline void tagUnlock()
      {
         if (s5clipper)
         {
            if ( tfile4lock( tagFile(), data4serverId( data4()) ) != 0 )
               throw Err5() ;
         }
         else
            if ( dfile4unlockIndex( data4()->dataFile, data4serverId( data4() ) ) != 0 )
               throw Err5() ;
      }
   private:
      long seek( const char *key, short len, int descend = 0  ) ;
      inline TAG4FILE *tagFile() { return data4()->tagSelected->tagFile ; }
   } ;
#endif /* S4STAND_ALONE */



#ifndef S4CLIENT
   class S5CLASS Table5schemaLow
   {
   public:
      #ifdef E4DEBUG
         Table5schemaLow() { holderForDebug = 1 ; }
      #endif

      virtual DATA4 *data4() = 0 ;
      virtual int goRead( ULONG recNo, Table5rowsetValues *rowsetValues = 0 ) = 0 ;
      virtual ULONG recNo() = 0 ;
      void rowRequestSchemaSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset ) ;
      void skip() ;
      inline long countAvailableRecords( LONG cRecNo )
      {
         int amtAvailable = tagSkip( cRecNo ) ;
         if ( amtAvailable < cRecNo )  // add 1 in order to include current record if can't get all requested
            amtAvailable++ ;
         return amtAvailable ;
      }
      long adjustCurrentRecNoForOffset( LONG offset, LONG endRecNo ) ;
      long tagSkip() ;
      long tagSkip( long numToSkip ) ;
      void tagSynch( ULONG synchRecNo ) ;
      char *rawRetrieve( ULONG recNo ) ;

      #ifdef E4DEBUG
         char holderForDebug ;
      #endif
   } ;
#endif



class Context4client
{
   // Basically this class is used to hang onto the calling client when we want to perform operations
   // using the catalog client as the driving object (but still know who the real 'current' client is).
public:
   #ifdef S4SERVER
      Context4client() { resetClient() ; }
      void resetClient() { client = 0 ; }
      void setClient( struct SERVER4CLIENTSt *clientIn )
      {
         assert5( clientIn != 0 ) ;
         // should never be setting to the catalog client - this is what we are trying to avoid with this class
         // - won't compile... assert5( clientIn->server->codeBase->catalogClient != clientIn ) ;
         client = clientIn ;
      }
      struct SERVER4CLIENTSt *getClient() { return client ; }
   #else
      void *getClient() { return 0 ; }
   #endif

private:
   #ifdef S4SERVER
      struct SERVER4CLIENTSt *client ;
   #endif
} ;



#ifdef S4STAND_ALONE
   class S5CLASS Table5schemaStand : public Table5schemaLow, public Table5schemaTag
   {
   public:
      // class to perform stand-alone operations for the Table5schema
      Table5schemaStand( Session5 *sessn ) : Table5schemaTag( sessn )
       {
          #ifdef E4DEBUG
             tableName = 0 ;
          #endif
          ;
       }
      // virtual ~Table5schemaStand() { ; }
      ~Table5schemaStand() { free5( tableName ) ; }
      virtual const char *name() = 0 ;

      #ifdef E4ANALYZE
         inline void doCheck()
         {
            static int count = 0 ;  // don't check every time or code runs too slow...
            if ( count++ >= 20 )
            {
               count = 0 ;
               if ( tran4active( data4()->codeBase, data4() ) == 0 )  // don't if in transaction or error occurs
               {
                  if ( d4check( data4() ) < 0 )   /* > 0 ok (eg. r4locked) */
                     throw Err5internal((data4())->codeBase) ;
               }
               else // AS 05/26/99 -- there is a chance here that an error occurred (active transaction)
               {
                  error4set( data4()->codeBase, 0 ) ;
               }
            }
         }
      #endif

      inline void log( int val )
      {
         if ( d4log( data4(), val ) < 0 )  // probably not enabled, just cancel error
            error4set( sess->c4, 0 ) ;
      }

      virtual void createSchemaTable() = 0 ;
      virtual void fieldInit() = 0 ;
      virtual void doChecks( Context4client *clientContext ) = 0 ;
      virtual void create() = 0 ;
      // AS 09/17/98 -- don't think needs to be virtual, avoid if possible...more efficient, this function called lots
      inline DATA4 *data4() { return Table5stand::data4() ; }
      inline void assignData4( DATA4 *dataIn ) { data = dataIn ; }
      COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) ;
      inline ULONG rowNumberRetrieve() { return Table5schemaTag::recNo() ; }
      int rowPosition( int cRowsOffset ) ;
      inline void rowRequestSchema( LONG cRowsOffset, LONG cRecNo ) { ; } // does nothing in stand-alone
      int rowSkip( LONG numSkip ) ;
      int setNextPosition() ;
      long seek( const char *key, short len, int descend = 0  ) ;
      int tagSelect( char *tagName, int *iRowset ) ;
      TAG4 *tag( char *tagName, int *iRowset ) ;
      inline void tagSelect( TAG4 *tag ) { d4tagSelect( data4(), tag ) ; }
      int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) ;
      void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) ; //, int *offsetRaw = 0 ) ;
      inline int goRead( ULONG recNum, Table5rowsetValues *rowsetValues = 0 )
         { return ( Table5low::recNo() == recNum ) ? 0 : Table5low::go( recNum, 1, rowsetValues, 0 ) ; }
      ULONG inline recNo() { return Table5schemaTag::recNo() ; }
      virtual WSTR5 *getName( int ordinal ) = 0 ;  // for long-name support, must override
      inline void rowRequestSequentialRepeat( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection )
      {
         throw Err5internal((data4())->codeBase) ;   // should never get here for schema, because all is handled at higher level
      }
      inline void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, int iRowset, char countDeletedAsSkip = 0 )
      {
         throw Err5internal((data4())->codeBase) ;   // should never get here for schema, because all is handled at higher level
      }
      inline void rowRequestSchemaSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, int iRowset )
      {
         Table5schemaLow::rowRequestSchemaSequential( startRecNo, offset, cRecNo, rowsetValues, iRowset ) ;
      }
      int open() ;
      inline char *addTag( TAG4INFO *tag )
      {
         throw Err5internal((data4())->codeBase) ;
         return 0 ;
      }  // cannot create indexes on schema rowsets, should never get here
      LPWSTR tableName ;
//      virtual inline int isSchemaTable() { return 1 ; }  // schema tables override
      virtual int defaultUniqueDesired() = 0 ;  // varies for different schema tables
   } ;
#endif /* S4STAND_ALONE */



// required for class below, so place in this file, not schema file
enum Schema5state
{
   schema5direct,
   schema5optimization,
   schema5eof,
   schema5done,
} ;



#ifdef S4SERVER
   class S5CLASS Table5schemaServer : public Table5server, public Table5schemaLow
   {
   public:
      Table5schemaServer( Source5all *src ) : Table5server( src, schema5true ) { ; }
      inline int goRead( ULONG recNo, Table5rowsetValues *rowsetValues = 0 )
         { return Table5low::go( recNo, 1, rowsetValues, 0 ) ; }
      inline int goWrite( ULONG recNo, Table5rowsetValues *rowsetValues = 0 )
         { return Table5low::go( recNo, 1, rowsetValues ) ; }
      inline DATA4 *data4() { return data4local() ; }
      inline ULONG recNo() { return Table5server::recNo() ; }
//      virtual inline int isSchemaTable() { return 1 ; }  // schema tables override
      virtual inline WSTR5 *getName( int ordinal ) { return (columns()[ordinal]).name ; }   // overriden by schema for long-name support
      long version ;
      virtual void createSchemaTable() = 0 ;
      void doCheck()
      {
         static int checkCount = 0 ;
         // AS 05/06/99 --> to make tests run faster, only check once in a while (say every 20th time...)
         if ( checkCount++ == 20 )
         {
            checkCount = 0 ;
            if ( d4check( data4() ) != 0 )
               throw Err5internal( data->codeBase ) ;
         }
      }
      short setRestrictions( long *endRecNo, long *startRecNo, const char *key, int keyLen, Schema5state *state ) ;
      inline DATA4 *data4local() { return data ; }
   } ;
#endif /* S4SERVER */



#ifdef S4SERVER
   class S5CLASS Table5schemaServerLocal : public Table5serverBase
   {
   public:
      Table5schemaServerLocal( Table5schemaServer *tbl ) { table = tbl ; }
      ~Table5schemaServerLocal() { table = 0 ; Table5serverBase::table = 0 ; }  // we are just a reference, don't delete the base table, so set table to NULL

      Table5schemaServer *table ;
   } ;
#endif /* S4SERVER */



#ifndef S4SERVER
   class S5CLASS Table5static : public Table5all
   {
      // static tables reside on client in c/s scenario
      // all static schema tables (array tables) use this table
   public:
      Table5static( Bool5 isSchemaTableIn ) : Table5all( isSchemaTableIn ) { ; }
      ~Table5static() ;

      // all static tables must implement the getFieldInfo function
      // virtual ULONG skipWithMatch( ULONG start, Bool5 forwardSkip ) = 0 ;
      virtual void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition = 0 ) = 0 ; //, int *offsetRaw = 0 ) = 0 ;
      virtual unsigned short numFields() = 0 ;
      virtual COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) = 0 ;
      virtual HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) = 0 ;
      virtual int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) = 0 ;

      virtual ULONG maxCount() = 0 ;
      ULONG skip( ULONG start, LONG numSkip, TAG4 *tagSelected ) ;
      inline ULONG recCount() { return maxCount() ; }   // for static tables == size of array...

      // static tables have set returns for the following
      virtual inline short numNullBytes() { return 0 ; }
      inline int isFieldDeferred( int iOrdinal ) { return 0 ; }
      inline void updateFieldset( Fieldset5all *fieldset, int *iRowset ) { ; }
      inline virtual void close() { ; }
      virtual inline const char *name() { return 0 ; }
      void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, int iRowset, char countDeletedAsSkip = 0 ) ;
      inline void rowRequestGetDeferredFields( Fieldset5all *fields, ULONG recNo, char **rawBuffer, ByRef5 *byRefBuffer, Table5rowsetValues *rowsetValues, int iRowset ) { ; }
      virtual int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) = 0 ;
      virtual void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) = 0 ;
      inline int isReadOnly( int iOrdinal ) { return 1 ; }  // always read-only
      inline int isStaticTable() { return 1 ; }
   } ;
#endif /* !S4SERVER */



#ifndef S4SERVER
   void update5tableVersion( Session5 *session, short id, long version ) ;
#endif

#endif /* OLEDB5BUILD */
