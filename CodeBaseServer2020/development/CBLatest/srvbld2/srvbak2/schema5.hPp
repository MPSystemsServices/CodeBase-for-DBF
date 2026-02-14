/* Schema5.hpp */

#ifdef OLEDB5BUILD

/*
// S4SERVER
Table5schemaBase::Table5schemaServer

// S4STAND_ALONE
Table5schemaBase::Table5schemaStand

// S4CLIENT
//Table5schemaBase::Table5schemaClient


Table5tables::Table5schemaBase

Table5columns::Table5schemaBase

Table5indexes::Table5schemaBase

Schema5tables::Schema5rowset::RowsetPlusSchema5

Schema5columns::Schema5rowset::RowsetPlusSchema5

Schema5indexes::Schema5rowset::RowsetPlusSchema5

Table5schemaProvider::Table5static

Schema5providerTypes::Schema5rowset::RowsetPlusSchema5

Basic set up is as follows (Stand-alone):

Source5all members: Table5tables
                    Table5indexes
                    Table5columns

The program has only one instance of Table5tables.  These tables are
used to perform internal operations (eg. adding entries to schema tables
when new tables are created, etc.).  In addition, the Schema Rowset objects
use these base tables to perform their operations.

Table5tables::Table5schemaBase::Table5schemaStand (see table5.hpp for this)

  -- remove this
Session5 members:   Schema5tables
                    Schema5indexes
                    Schema5columns


When requesting schema rowsets, users receive one of Schema5tables, Schema5indexes,
  Schema5columns, etc.

Schema5tables::Schema5rowset::RowsetPlusSchema5::Rowset5all::Row5bufControl

*/


enum Schema5status
{
   schema5statusComplete, // schema5statusComplete - 'nPtrs' is all that is possibly there.
                     // Every thing is complete except for level entries
                     // marked with schema5statusException.
   schema5statusException, // schema5statusException - In this case 'ptr' can be null.
                     // This is only used on the level data chunks are read in at.
   schema5statusMore, // schema5statusMore - There might be more data after 'n'
                     // Level 1 is always STATUS5COMPLETE &
                     // for Level 0, this is of course not relevant.
   schema5statusNull,   // set when we are not in a valid state at all
} ;




#ifndef S4SERVER

/* !S4SERVER, OLEDB5BUILD */
class Schema5rowset : public RowsetPlusSchema5
{
   /* we will disallow backwards skipping on the schema rowsets
      This class corresponds to a particular instance the schema rowset
      is opened.  If a particular schema rowset is opened twice for the same
      table, the optimization buffers are locked by one of them until
      that rowset is done.
   */
public:
   DECLARE5_IRowsetIdentity

   Schema5rowset( Session5 *sess ) ;
   ~Schema5rowset() ;

   virtual int createAndSelectRestrictionIndexString( int nRestrictions, const VARIANT *restrictions ) ;
   virtual void createKey() = 0 ;
   inline void freeRow( Row5all *row ) { RowsetPlusSchema5::freeRow( row ) ; }
//   virtual void freeRow( HROW ) = 0 ;  // delete function must be specific to Row5/Row5index
   HRESULT getHandles( LONG offset, LONG cRecno, ULONG *nRowsObtained, Row5all **hRows, Bool5 updateNextRecNo, char countDeletedAsSkip ) ;
   virtual VARIANT getRestrictionValue( const char *buffer, int restrictionNum ) = 0 ;
   virtual HRESULT indexSelect( int nRestrictions, const VARIANT *restrictions ) = 0 ;
   virtual int init() = 0 ;
   virtual HRESULT populateFromSelectedRestrictions() { return S_OK ; }   // not always overridden (eg. provider rowset)
   virtual void prefix( char *copyTo ) = 0 ;
   virtual int prefixLen() = 0 ;
   virtual void remove() = 0 ;
   HRESULT STDMETHODCALLTYPE RestartPosition( HCHAPTER hReserved) ;
   void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char countDeletedAsSkip = 0 ) ;
   HRESULT saveRestrictions( ULONG cRestrictions, const VARIANT *restrictions ) ;
   virtual Schema5type schemaType() = 0 ; // { throw Err5internal() ; return schema5typeInvalid ; }  // gets overriden if required to implement (i.e. non-provider)
   virtual int rowRefetch( ULONG recNo, char **raw, ULONG *recNoOut ) ;
   #ifdef S4CLIENT
      virtual long seek( char *key, short len ) { throw Err5internal( 0 ) ; }  // should only be called for static tables where this is overridden
   #else
      virtual long seek( char *key, short len ) ;
   #endif
   virtual int seek( const WSTR5 *seek, int numSeekBytes ) ;
   HRESULT setRestrictions( ULONG nRestrictions, const VARIANT *restrictions ) ;
   HRESULT determineEndPoints() ;
   ULONG startOrdinal() { return 1 ; }   // schema rowsets don't have bookmark fields...
   virtual void tagSelect( char *tagAlias ) ;   // provider overrides
   Schema5tables *getTablesSchemaRowset() ;
   #ifdef S4STAND_ALONE
      virtual void tagSelect( TAG4 *tag ) ;   // provider overrides
   #endif
   #ifdef S4OLEDEBUG_PRINT
      virtual const char *rowsetName() = 0 ;
   #endif
   inline char verifyRowInRange( char *raw, long recNo, Table5rowsetValues *rowsetValues )
   {
      if ( nextIsDone == 1 )  // we are on done now
         return 0 ;
      // AS must be an == comparison only because we may be using tags, in which case the
      // records do not come out in record number ascending or descending order...
      // AS 08/27/99 -- the problem is that with static tables, the 'recno' returned is
      // 1 > the 'setEndRecno' because the 'setEndRecno' == array entry #.
      // therefore, relax the constraint on static tables...

      if ( accessorHandler.tableAll->isStaticTable() )
      {
         if ( rowsetValues->setEndRecno == (unsigned long)recNo - 1 )
            nextIsDone = 1 ;
      }
      else
      {
         if ( rowsetValues->setEndRecno == (unsigned long)recNo )
            nextIsDone = 1 ;
      }
      return 1 ;
   }

   Fieldset5all *fieldset ;  // for the schema this will always be the entire record, and there
                          // will be no deferred data or interfaces
   int nRestrictions ; // Number of restrictions with no filter aspect involved.
                       // As soon as the data value changes
   VARIANT *restrictions ; // The current set of restriction data.  Actually, I do not think we need
                           // to permanently store this information.  Perhaps in case where optimization
                           // buffers cannot be used this info. is needed to be kept.
   ULONG savedNumRestrictions ;
   VARIANT *savedRestrictions ;
   Schema5state state ; // schema5optimization  In the middle of reading the data using the optimization
                        // schema5direct        Can't use optimization
                        // schema5done          Done reading data.  Nothing more available for this set of restrictions.
                        // schema5eof           At end of state (no records found, but not 'done' yet - after initialization
   char *tagAlias ;
   long version ;

   const WSTR5 *getCurrentDirectoryUnicode() ;
   void setEof() ;

protected:
   char *key ;
   short keyLen ;
   #ifndef S4CLIENT
      TAG4 *restrictionTag ;
   #endif
   char nextIsDone ;   // used by the provider types rowset, so expose as protected
private:
   #ifdef S4CLIENT
      virtual long getLastRecno( char *key, short len ) { throw Err5internal( 0 ) ; }  // should only get called for use with static tables, where this is overridden
   #else
      virtual long getLastRecno( char *key, short len ) ;
   #endif
   WSTR5 *curDirectoryUnicode ;
} ;

#endif /* !S4SERVER */



// these defines are used to tell the current position if not at a valid record number
// they are stored in currentRecno, and must be negative so as not to conflict with
// valid record numbers.
#define POSITION5INVALID -1
#define POSITION5EOF -2
#define POSITION5BOF -3
#define POSITION5UNKNOWN -4



#ifndef S4CLIENT

struct SERVER4CLIENTSt ;

/* !S4CLIENT, OLEDB5BUILD */
#ifdef S4SERVER
class Table5schemaBase : public Table5schemaServer
#endif
#ifdef S4STAND_ALONE
class Table5schemaBase : public Table5schemaStand
#endif
{
public:
   #ifdef S4SERVER
      Table5schemaBase( Source5all *src, Schema5type schId ) : Table5schemaServer( src ) { ; }
   #endif
   #ifdef S4STAND_ALONE
      Table5schemaBase( Session5 *sess, Schema5type schId ) : Table5schemaStand( sess ) { ; } // src = sess->source ; }
   #endif

   void createSchemaTable() ;   // create function common to both server and stand/alone
   virtual void create() = 0 ;
   virtual void fieldInit() = 0 ;
   virtual void doChecks( Context4client *clientContext ) = 0 ;
   inline virtual int init( Context4client *clientContext )
   {
      // Any initialization; -1 error
      #ifdef S4SERVER
         return open( clientContext ) ;
      #else
         return open() ;
      #endif
   }
   int isSchemaTable() { return 1 ; }  // schema tables override
   void verifyOpenTables( Context4client *clientContext ) ;
   virtual void check( Table5low *data ) = 0 ;
   virtual int defaultUniqueDesired() = 0 ;  // varies for different schema tables

   #ifdef S4SERVER
      inline COLUMN5INFO *getColumns( short *nColumns, int *freeRequired )
      {
         COLUMN5INFO *colsOut = d5columns( data, nColumns, 1, src ) ;
         if ( colsOut != 0 )
            *freeRequired = 1 ;
         return colsOut ;
      }
      int open( Context4client *clientContext ) ;
   #endif
} ;


/* !S4CLIENT, OLEDB5BUILD */
class Table5tables: public Table5schemaBase
{
public:
   #ifdef S4SERVER
      Table5tables( Source5all *src ) : Table5schemaBase( src, schema5typeTables )
      {
          source = src ;
          tblCatalog = tblSchema = tblName = tblType = tblGuid = description = regenCols = regenIdx = tblPropId = dtCreated = dtModified = 0 ;
          #ifdef E4DEBUG
             #ifdef S4SERVER
               memset( currentFileName, 0, LEN4PATH ) ;
            #else
               memset( currentFileNameUnicode, 0, LEN4PATH ) ;
            #endif
          #endif
      }
   #else
      Table5tables( Session5 *sess ) : Table5schemaBase( sess, schema5typeTables )
      {
          source = sess->source ;
          tblCatalog = tblSchema = tblName = tblType = tblGuid = description = regenCols = regenIdx = tblPropId = dtCreated = dtModified = 0 ;
          #ifdef E4DEBUG
             #ifdef S4SERVER
               memset( currentFileName, 0, LEN4PATH ) ;
            #else
               memset( currentFileNameUnicode, 0, LEN4PATH ) ;
            #endif
          #endif
      }
   #endif

   void addDirectory( const WSTR5 *directoryName ) ;  // used to add the contents of a directory to the table
   void addEntry( const WSTR5 *catName, const WSTR5 *tableName, char isSystem ) ;
   inline void check( Table5low *data ) { addTable( data ) ; }
   void addTable( Table5low *table ) ;  // used to add the contents of a table to the table
   void create() ;
   void fieldInit() ;
   WSTR5 *getName( int ordinal ) ;
   inline COLUMN5INFO *getColumns( short *nColumns, int *freeRequired )
   {
      COLUMN5INFO *colsOut = d5columns( data, nColumns, 1, source, 2 ) ;
      if ( colsOut != 0 )  // don't set freeRequired unless there really is something to free...
         *freeRequired = 1 ;
      return colsOut ;
      // 2 extra columns (regen stuff), don't expose
   }
   void doChecks( Context4client *clientContext ) ;
   void findOrAddEntry( const WSTR5 *catName, const WSTR5 *tableName ) ;
   const char *name() { return "TABLES5.DBF" ; }  // need extension for adding to schema tables
   int regenColumns( LPCWSTR tableFullPath ) ;
   void regenColumnsSetFalse( LPCWSTR tableFullPath ) ;
   int regenIndex( LPCWSTR tableFullPath ) ;
   void regenIndexSetFalse( LPCWSTR tableFullPath ) ;
   void regenIndexSetTrue( LPCWSTR tableFullPath ) ;
   void removeTable( Table5low *table ) ;
   int defaultUniqueDesired() { return e4unique ; }
   void populateIndexesRowset( const WSTR5 *catalogKey, const WSTR5 *tableKey = 0 ) ;
   void populateColumnsRowset( const WSTR5 *catalogKey, const WSTR5 *tableKey = 0 ) ;
   inline unsigned short numRegisteredFields() { return numFields() - 2 ; }  // don't include the REGEN fields in the info...

   FIELD4 *tblCatalog, *tblSchema, *tblName, *tblType, *tblGuid, *description, *regenCols, *regenIdx, *tblPropId, *dtCreated, *dtModified ;
   Source5all *source ;
private:
   #ifdef S4SERVER
      const char *getCurrentFileName() ;
      char currentFileName[LEN4PATH] ;
   #else
      const WSTR5 *getCurrentFileNameUnicode() ;
      WSTR5 currentFileNameUnicode[LEN4PATH] ;
   #endif
} ;

#endif /* !S4CLIENT */



#ifndef S4SERVER

#define DBSCHEMA5TABLES_TABLE_CATALOG  0
#define DBSCHEMA5TABLES_TABLE_SCHEMA   1
#define DBSCHEMA5TABLES_TABLE_NAME     2
#define DBSCHEMA5TABLES_TABLE_TYPE     3



/* !S4SERVER, OLEDB5BUILD */
class Schema5tables : public Schema5rowset
{
public:
   void * operator new( size_t s ) ;
   void * operator new( size_t s, Session5 *sess ) ;
   void operator delete( void *p ) { free5(p) ; }
   // AS 09/22/98 delete here required in case of exception in operator new to allow
   //    freeing of memory (VC 6.0)
   void operator delete( void *p, Session5 *sess ) { free5(p) ; }

   Schema5tables( Session5 *sess ) : Schema5rowset( sess ) { ; }

   int init() ;
   DECLARE5_IUnknown
   // don't use special memory allocations since if a Table5schema is allocated once within a
   // session it is not freed until the session is over

//   inline void freeRow( HROW row ) { RowsetPlusSchema5::freeRow( row ) ; }

   HRESULT indexSelect( int nRestrictions, const VARIANT *restrictions ) ;  // each schema table must implement its own version of indexSelect
   void createKey() ;
   inline void remove() { delete this ; }

   inline int prefixLen() { return 3 ; }
   inline void prefix( char *copyTo ) { memcpy( copyTo, "T5T", 3 ) ; }
   Schema5type schemaType() { return schema5typeTables ; }

   void addDirectory( const WSTR5 *directoryName ) ;  // used to add the contents of a directory to the table

   // not coded yet...
   int classLock() { if ( classLocked != 0 ) return 0 ; classLocked = 1 ; return 1 ; }
   void classUnlock() { classLocked = 0 ; }
   #ifndef S4OFF_SCHEMA_OPTIMIZE
      int optimizedSortSupported() ;
   #endif
   VARIANT getRestrictionValue( const char *buffer, int restrictionNum ) ;
   short numOptimizedRestrictions() { return 3 ; }
   void findOrAddEntry( const WSTR5 *catName, const WSTR5 *tableName ) ;
   void populateFromCatalogKey( const WSTR5 *catalogKey, const WSTR5 *tableKey = 0 ) ;
   void populateIndexesRowset( const WSTR5 *catalogKey, const WSTR5 *tableKey = 0 ) ;
   void populateColumnsRowset( const WSTR5 *catalogKey, const WSTR5 *tableKey = 0 ) ;

   #ifndef S4CLIENT
      Table5tables *tablesTable ;
   #endif

   #ifdef S4OLEDEBUG_PRINT
      const char *rowsetName()
      {
         static char rowsetNameForLog[] = "Schema Rowset: Tables" ;
         return rowsetNameForLog ;
      }
   #endif

private:
   static int classLocked ;
} ;

#endif /* !S4SERVER */


#ifndef S4CLIENT

/* !S4CLIENT, OLEDB5BUILD */
class Table5columns: public Table5schemaBase
{
public:
   #ifdef S4SERVER
      Table5columns( Source5all *src ) : Table5schemaBase( src, schema5typeColumns )
      {
//         source = src ;
   #else
      Table5columns( Session5 *sess ) : Table5schemaBase( sess, schema5typeColumns )
      {
//         source = sess->source ;
   #endif
      tableCatalog = tblSchema = tblName = colName = colGuid = colPropId = ordinal =
      colHasDef = colDef = colFlags = isNullable = dataType = typeGuid = chrMaxLen =
      chrOctLen = numPrec = numScale = dateTimePrec = charSetCatalog = charSetSchema =
      charSetName = collationCatalog = collationSchema = collationName = domainCatalog =
      domainSchema = domainName = description = 0 ;
   }

   void registerField( LPCWSTR tblCatalog, LPCWSTR tName, LPCWSTR colNameIn, const int fieldNumber,
                       const unsigned long flags, const char isNullIn, const long dataTypeIn,
                       const long maxLen, const unsigned long ocLen, const int prec, const int scale, const int dtTimePrec ) ;

   void registerFieldFromField4( FIELD4 *field, LPCWSTR tblCatalog, LPCWSTR tName, const int fieldNumber, LPCWSTR colmName, Bool5 isSchema ) ;
   WSTR5 *getName( int ordinal ) ;

   int regenerateTable( Table5low *data, int markRegenFlag = 1 ) ;
   void check( Table5low *data ) ;
   void doChecks( Context4client *clientContext ) ;
   void fieldInit() ;
   void create() ; //  does the actual table creation.
   void removeTable( Table5low *table ) ;
   const char *name() { return "COLUMNS5.DBF" ; }  // need extension for adding to schema tables
   int defaultUniqueDesired() { return e4unique ; }
   Bool5 seekForTable( const WSTR5 *catName, const WSTR5 *tableName ) ;

//   Source5all *source ;
   FIELD4 *tableCatalog, *tblSchema, *tblName, *colName, *colGuid, *colPropId, *ordinal,
          *colHasDef, *colDef, *colFlags, *isNullable, *dataType, *typeGuid, *chrMaxLen,
          *chrOctLen, *numPrec, *numScale, *dateTimePrec, *charSetCatalog, *charSetSchema,
          *charSetName, *collationCatalog, *collationSchema, *collationName, *domainCatalog,
          *domainSchema, *domainName, *description ;
} ;
#endif /* !S4CLIENT */

#ifndef S4SERVER

#define DBSCHEMA5COLUMNS_TABLE_CATALOG  0
#define DBSCHEMA5COLUMNS_TABLE_SCHEMA   1
#define DBSCHEMA5COLUMNS_TABLE_NAME     2
#define DBSCHEMA5COLUMNS_COLUMN_NAME    3



/* !S4SERVER, OLEDB5BUILD */
enum Column5restrictionsEnum
{
   catalogNameRestrictionClm,
   tableNameRestrictionClm,
   columnNameRestrictionClm,
} ;



/* !S4SERVER, OLEDB5BUILD */
class Schema5columns : public Schema5rowset
{
public:
   void * operator new( size_t s ) ;
   void * operator new( size_t s, Session5 *sess ) ;
   void operator delete( void *p ) { free5(p) ; }
   // AS 09/22/98 delete here required in case of exception in operator new to allow
   //    freeing of memory (VC 6.0)
   void operator delete( void *p, Session5 *sess ) { free5(p) ; }
   Schema5columns( Session5 *sess ) : Schema5rowset( sess ) { ; } // { isCached = 0 ; }
   DECLARE5_IUnknown
   // don't use special memory allocations since if a Table5schema is allocated once within a
   // session it is not freed until the session is over

   // This class is used for the columns5.dbf schema rowset

   /* Regenerate in the following instances:
      - when table is first opened, may need to regenerate
      - for any given table, when it is opened, compare record-len and # fields, and ensure
        that they match the Columns5 schema.  If they do not, then regenerate the columns
        for that table -- also need to regenerate table entry
   */
//   inline void freeRow( HROW row ) { RowsetPlusSchema5::freeRow( row ) ; }

   HRESULT indexSelect( int nRestrictions, const VARIANT *restrictions ) ;
   int init() ;

   Schema5type schemaType() { return schema5typeColumns ; }
   void createKey() ;
   inline void remove() { delete this ; }
   inline int prefixLen() { return 3 ; }
   inline void prefix( char *copyTo ) { memcpy( copyTo, "C5T", 3 ) ; }
   HRESULT populateFromSelectedRestrictions() ;
   isEmpty( Column5restrictionsEnum restrictionId ) ;
   inline isSet( Column5restrictionsEnum restrictionId ) { return !isEmpty( restrictionId ) ; }
   const WSTR5 *getRestrictionV_BSTR( Column5restrictionsEnum restrictionId ) ;
   // not coded yet...
   int classLock() { if ( classLocked != 0 ) return 0 ; classLocked = 1 ; return 1 ; }
   void classUnlock() { classLocked = 0 ; }
   #ifndef S4OFF_SCHEMA_OPTIMIZE
      int optimizedSortSupported() ;
   #endif
   VARIANT getRestrictionValue( const char *buffer, int restrictionNum ) ;
   #ifndef S4CLIENT
      Table5columns *columnsTable ;
   #endif
   #ifdef S4OLEDEBUG_PRINT
      const char *rowsetName()
      {
         static char rowsetNameForLog[] = "Schema Rowset: Columns" ;
         return rowsetNameForLog ;
      }
   #endif
private:
   static int classLocked ;
} ;

#endif /* !S4SERVER */




// we use this special character to identify non-integrated indexes in indexes schema table...
#define NON5INTEGRATED_PRECEDING_CHARACTER '&'
#define NON5INTEGRATED_PRECEDING_CHARACTER_WIDE L'&'



#ifndef S4CLIENT

/* !S4CLIENT, OLEDB5BUILD */
class Table5indexes: public Table5schemaBase
{
public:
   #ifdef S4SERVER
      Table5indexes( Source5all *src ) : Table5schemaBase( src, schema5typeIndexes )
      {
   #else
      Table5indexes( Session5 *sess ) : Table5schemaBase( sess, schema5typeIndexes )
      {
   #endif
      tableCatalog = tableSchema = tableName = indexCatalog =
      indexSchema = indexName = primaryKey = unique = clustered = iType = fillFactor =
      initSize = nulls = sortBookmark = autoUpdate = nullCollation = ordinalPosition =
      columnName = columnGuid = columnPropertyId = collation = cardinality = pages =
      filterCondition = integrated = isComplex = 0 ;
   }
   void create() ;
   void check( Table5low *table ) ;  // adds all indexes for a table
   void doChecks( Context4client *clientContext ) ;
   void fieldInit() ;
   const char *name() { return "INDEXES5.DBF" ; }  // need extension for adding to schema tables
   void regenerateTable( Table5low *table, int markRegenFlag = 1 ) ;  // adds all indexes for a table
   void addIndex( Index5 *index ) ;  // adds a single index for a table
   void removeTable( Table5low *table ) ;
   void registerField( LPCWSTR tblCatalog, LPCWSTR tName, TAG4 *tag, int isCmplex, int fieldNumber, LPCWSTR fieldName, Bool5 isIntegrated ) ;
   void regenerateTableMakeFieldNameComplex( char *inputName, LPWSTR outputNameWide ) ;
   int defaultUniqueDesired() { return e4unique ; }
   WSTR5 *getName( int ordinal ) ;
   Bool5 seekForTable( const WSTR5 *catName, const WSTR5 *tableName ) ;

   FIELD4 *tableCatalog, *tableSchema, *tableName, *indexCatalog,
          *indexSchema, *indexName, *primaryKey, *unique, *clustered, *iType, *fillFactor,
          *initSize, *nulls, *sortBookmark, *autoUpdate, *nullCollation, *ordinalPosition,
          *columnName, *columnGuid, *columnPropertyId, *collation, *cardinality, *pages,
          *filterCondition, *integrated, *isComplex ;
} ;

#endif /* !S4CLIENT */



#ifndef S4SERVER

/* !S4SERVER, OLEDB5BUILD */
enum Index5restrictionsEnum
{
   catalogNameRestrictionIdx,
   tableNameRestrictionIdx,
   indexNameRestrictionIdx,
} ;



// The input ordering of restrictions to SetRestrictions() (from OLE-DB specs)
#define DBSCHEMA5INDEXES_TABLE_CATALOG  0
#define DBSCHEMA5INDEXES_TABLE_SCHEMA   1
#define DBSCHEMA5INDEXES_INDEX_NAME     2
#define DBSCHEMA5INDEXES_TYPE           3
#define DBSCHEMA5INDEXES_TABLE_NAME     4



/* !S4SERVER, OLEDB5BUILD */
class Schema5indexes : public Schema5rowset
{
public:
   void * operator new( size_t s ) ;
   void * operator new( size_t s, Session5 *sess ) ;
   void operator delete( void *p ) { free5(p) ; }
   // AS 09/22/98 delete here required in case of exception in operator new to allow
   //    freeing of memory (VC 6.0)
   void operator delete( void *p, Session5 *sess ) { free5(p) ; }
   Schema5indexes( Session5 *sess ) : Schema5rowset( sess ) { ; } // { isCached = 0 ; }
   DECLARE5_IUnknown
   // don't use special memory allocations since if a Table5schema is allocated once within a
   // session it is not freed until the session is over

//   inline void freeRow( HROW row ) { RowsetPlusSchema5::freeRow( row ) ; }
   int init() ;
   isEmpty( Index5restrictionsEnum restrictionId ) ;
   inline isSet( Index5restrictionsEnum restrictionId ) { return !isEmpty( restrictionId ) ; }
   const WSTR5 *getRestrictionV_BSTR( Index5restrictionsEnum restrictionId ) ;

   HRESULT indexSelect( int nRestrictions, const VARIANT *restrictions ) ;
   void createKey() ;
   inline void remove() { delete this ; }
   int createAndSelectRestrictionIndexString( int nRestrictions, const VARIANT *restrictions ) ;
   inline int prefixLen() { return 3 ; }
   inline void prefix( char *copyTo ) { memcpy( copyTo, "I5T", 3 ) ; }
//   int seek( const WSTR5 *seek, int numSeekBytes ) ;
   Schema5type schemaType() { return schema5typeIndexes ; }
   HRESULT populateFromSelectedRestrictions() ;

   // not coded yet...
   int classLock() { if ( classLocked != 0 ) return 0 ; classLocked = 1 ; return 1 ; }
   void classUnlock() { classLocked = 0 ; }
   #ifndef S4OFF_SCHEMA_OPTIMIZE
      int optimizedSortSupported() ;
   #endif
   VARIANT getRestrictionValue( const char *buffer, int restrictionNum ) ;
   #ifndef S4CLIENT
      Table5indexes *indexesTable ;
   #endif
   #ifdef S4OLEDEBUG_PRINT
      const char *rowsetName()
      {
         static char rowsetNameForLog[] = "Schema Rowset: Indexes" ;
         return rowsetNameForLog ;
      }
   #endif
private:
   static int classLocked ;
} ;



/* !S4SERVER, OLEDB5BUILD */
typedef struct
{
   // AS 08/27/99 had problems, made just a simple widestring...
   // WSTR5 typeName[LEN5MAX_TYPE_NAME+2] ;  // 2 bytes for null, 2 bytes for length store
   WSTR5 typeName[LEN5MAX_TYPE_NAME+1] ;  // 2 bytes for null,
   // AS 05/15/99 stored as ui2... not DBTYPEENUM
   // DBTYPEENUM dataType ;   // sort by this
   unsigned short dataType ;
   ULONG columnSize ;
   unsigned short literalPrefix[1] ;   // always null
   unsigned short literalSuffix[1] ;   // always null
   unsigned short createParams[LEN5MAX_CREATE_PARAMS+2] ;  // 2 bytes for null, 2 bytes for length store
   VARIANT_BOOL isNullable ;
   VARIANT_BOOL caseSensitive ;
   ULONG searchable ;
   VARIANT_BOOL unsignedAttribute ;
   VARIANT_BOOL fixedPrecScale ;
   VARIANT_BOOL autoUniqueValue ;
   WSTR5 localTypeName[LEN5MAX_LOCAL_TYPE_NAME+1] ;
   SHORT minimumScale ;
   SHORT maximumScale ;
   GUID guidNull ;
   unsigned short typeLib[LEN5MAX_TYPE_LIB+1] ;    // unsure as to use, not documented well by OLE-DB
   unsigned short version[LEN5MAX_VERSION+1] ;
   VARIANT_BOOL isLong ;
   VARIANT_BOOL bestMatch ;     // restrict by this (i.e. subsort)
   VARIANT_BOOL isFixedLength ;
   long nulls ;           // need to match FoxPro nulls. must go after best match which is last documented field
   char  internalType ;       // not exported as a column name, but used internally
   char  rawType ;       // not exported as a column name, but used internally
} Provider5typeTable ;



class Schema5providerTypes ;

/* !S4SERVER, OLEDB5BUILD */
class Table5schemaProvider : public Table5static
{
   // Schema5provider uses a static array table
public:
   Table5schemaProvider( Schema5providerTypes *providerIn ) : Table5static( 1 ) { providerTypes = providerIn ; }
   void getFieldInfo( unsigned int iOrdinal, int *lenField, int *lenFieldDec, Type5 *typeInternal, int *isNullBitPosition ) ; //, int *offsetRaw ) ;
   COLUMN5INFO *getColumns( short *nColumns, int *freeRequired ) ;
   unsigned short numFields() ;
   virtual inline int isSchemaTable() { return 1 ; }  // schema tables override
   HRESULT mapColumnIds( unsigned long nIds, const struct tagDBID columnNames[], unsigned long columndIds[] ) ;
   int rowRequestGetNext( char **rawBuffer, ULONG *recNo, int direction, Table5rowsetValues *rowsetValues, Fieldset5all *fieldsetCurrent, Bool5 updateToNextPosition ) ;
   inline void removeIndexes() { throw Err5internal(0) ; }  // no indexes to be removed, better not get called!
   inline int numDeferredFields() { return 0 ; }  // provider has no deferred fields
   inline short numNullBytes() { return 1 ; }  // AS 12/07/00 the provider has 1 byte of null values
   ULONG maxCount() ;
   inline void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char setOldDirection, char countDeletedAsSkip = 0 )
   {
      Table5static::rowRequestSequential( startRecNo, offset, cRecNo, rowsetValues, setOldDirection, countDeletedAsSkip ) ;
      // returnCount =  ;... next line, no longer used, now use rowset values value
      #ifdef S4STAND_ALONE
         rowsetValues->count = min( (long)abs( cRecNo ), (long)(rowsetValues->setEndRecno - rowsetValues->setStartRecno + 1) ) ;
      #endif
   }
   Provider5typeTable *provider5types ;
   int provider5typesSize ;
   short iDataGet() { throw Err5internal(0) ; return -1 ; }  // doesn't apply to provider table
   int getOffsetValue( const Fieldset5all *fieldsetIn, const Field5all *fieldOn, int *offset ) ;
   void setNullBytesOffset( Fieldset5all *fieldsetIn, int offset ) ;
private:
   Schema5providerTypes *providerTypes ;  // AS 12/08/00 need for checking restrictions on bestMatch only restriction
//   unsigned long returnCount ;   // # records to return from request
} ;



/* !S4SERVER, OLEDB5BUILD */
class Schema5providerTypes : public Schema5rowset
{
public:
   DECLARE5_IColumnsInfo
   DECLARE5_IRowsetIdentity

   void * operator new( size_t s ) { void *ptr = alloc5(s) ; memset( ptr, 0, s); return ptr ; }
   void operator delete( void *p ) { free5(p) ; }
   Schema5providerTypes( Session5 *sess ) ;
   ~Schema5providerTypes() ;
   const char *getKey() { return key ; }
   DECLARE5_IUnknown
//   inline void freeRow( HROW row ) { RowsetPlusSchema5::freeRow( row ) ; }
   #ifndef S4CLIENT
      void create() { throw Err5internal(session->c4) ; }  // should never get called
   #endif
   inline void remove() { delete this ; }
   #ifndef S4CLIENT
      inline void fieldInit() { ; }   // no fields, so does nothing
   #endif
   // no index, but must save restrictions in index select function
//   inline HRESULT indexSelect( int nRestrictions, const VARIANT *restrictions ) { return S_OK ; }   // no indexes in sorted table
   HRESULT indexSelect( int nRestrictions, const VARIANT *restrictions ) ;
   Schema5type schemaType() { return schema5typeProvs ; }
   void createKey() ;
   long seek( char *key, short len ) ;
   long getLastRecno( char *key, short len ) ;
   int isFieldDeferred( int iOrdinal ) { return 0 ; }    // no fields are deferred in provider types
   int init() ;
   inline int prefixLen() { throw Err5internal(session->c4) ; return 0 ; }  // doesn't use tags
   inline void prefix( char *copyTo ) { throw Err5internal(session->c4) ; }  // doesn't use tags
   void rowRequestSequential( ULONG startRecNo, LONG offset, LONG cRecNo, Table5rowsetValues *rowsetValues, char countDeletedAsSkip = 0 ) ;
   inline void tagSelect( char *tagAlias ) { ; }
   int rowRefetch( ULONG recNo, char **raw, ULONG *recNoOut ) ;
   #ifndef S4CLIENT
      inline void tagSelect( TAG4 *tag ) { ; }
   #endif
   inline ULONG baseTableCount() { return provTable->recCount() ; }  // overriddent for static tables....

   int classLock() { return 0 ; }    // static table, bypass
   void classUnlock() { ; }
   #ifndef S4OFF_SCHEMA_OPTIMIZE
      int optimizedSortSupported() { return 0 ; }
   #endif
   VARIANT getRestrictionValue( const char *buffer, int restrictionNum ) ;
   #ifdef S4OLEDEBUG_PRINT
      const char *rowsetName()
      {
         static char rowsetNameForLog[] = "Schema Rowset: Provider Types" ;
         return rowsetNameForLog ;
      }
   #endif
     // static table, bypass
   Bool5 hasBestMatchRestriction, hasTypeRestriction ;
private:
   Table5schemaProvider *provTable ;
   short curRecNo ;
   Provider5typeTable *provider5types ;
   int provider5typesSize ;
   // ULONG skipWithMatch( ULONG start, Bool5 forwardSkip ) ;
} ;



char provs5rawType( CODE4 *c4, LPCWSTR typeName ) ;
char provs5rawType( CODE4 *c4, DBTYPEENUM wType, Column5properties *properties ) ;

HRESULT VariantCopy5( VARIANTARG FAR *dest, const VARIANTARG FAR *source ) ;

#endif /* !S4SERVER */


char is5systemTable( const char *fileName, CODE4 *c4 ) ;

#endif /* OLEDB5BUILD */
