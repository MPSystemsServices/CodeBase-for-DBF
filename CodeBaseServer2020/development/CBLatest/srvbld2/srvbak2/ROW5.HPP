// row5.hpp

#ifdef OLEDB5BUILD

#ifndef S4SERVER

#include <list>

using namespace std ;

// #define pRow5(p) (Row5 *)(p) Must be consistent about casting back & forth.
// #define pRow5all(p) (Row5all *)(p)
inline Row5all *pRow5all( HROW h ) { return (Row5all *) h ; }
inline Row5all *pRow5all( Link5 *h ) { return (Row5all *) h ; }
inline Row5all **ppRow5all( HROW *h ) { return (Row5all **) h ; }
inline HROW HROW5( Row5all *row ) { return (HROW)row ; }

class Fieldset5 ;
class Rowset5index ;
class RowsetPlusSchema5 ;
/* Index5fieldInfo describes the Key
Rules for determining the field name.

** note there is one problem with the following algorithm: if a field name is used twice
   within the same expression, we may have 2 indistinguishable field names.  This is not
   too critical since the field ordinals will be different.

- If the expression is just a field name, use it.
- If the expression is a '+' string concatenation of subexpressions, generate
corresponding field names:
   - If it is just a Character Field, use the actual field name.
   - If it is a STR conversion of a Numeric field with no loss of data,
   use the Numeric field name.
   - if it is just a descending tag, use the field name
   - If it is a different conversion of a different field type with no
   loss of data, use that field name. (Identify any of these cases.)
   - If it is an otherwise complex sub-expression, generate a field name:
   COMPLEX_EXPR1, COMPLEX_EXPR2 .. and so on.
   - If it is a date field converted with DTOS() just use the field name

   - if it is RECNO(), display as the bookmark field - field name extracted from config.dbf
     field 'RECNO_NAME', default is RECNO5

   - if it is .DELETED() ??? problems because logical whereas the field contents are '*' or blank.

- If the expression is otherwise more complex than we want to handle,
generate a single field name for the expression using the tag name prefixed by "TAG".
Also, if there is a filter expression other than .NOT. DELETED(), generate
a tag name in this manner.

- descending tags also cause all expressions to become complex

Special implementation notes:

With complex expressions, it is impossible to obtain speed advantages by making use of the
  range information for positioning since the complex expression can cause any key to be
  anywhere within the range from a 'field' point of view.

Special optimizations in the future could be made for the following:
  descending tags

*/

// For Row5contents only
// when transferring a field fails, a StatusData5 is set up for the failed
// field, and added to the Row5 pStatus ;
// S4CLIENT, S4STAND_ALONE
/*
class5 Status5
{
   // since a given field error will rarely, if ever, happen, do not do any sort of special memory
   // allocation here.
   int ordinal ;
   DBSTATUS status ;
   // KS I don't think we need to bother with this here.  setStatus( int fOrdinal, StatusType sType ) { fieldOrdinal = fOrdinal ; status = sType ; )
} ;

class5 Status5list
{
   // Null 'Status5 *' is returned to signal there are no more error status values.
   Status5 *getStatusValueFirst( long *workingMemory ) { Make this one inline please }
   Status5 *getStatusValueNext( long *workingMemory ) ;
   void addStatusError( int fOrdinal, DBSTATUS sType ) ;  // add an error to the array of status

   // pStatusArray can be several possibilities, depending on error condition:
   //   will point to NULL if no errors
   //   will point to RowSet5.deletedStatus if r4deleted occurred for the row.  An r4deleted
   //     is indicated by fieldOrdinal set to STATUS5RECORD_DELETED
   //   will point to RowSet5.lockedStatus if r4locked occurred for the row.  An r4locked
   //     is indicated by fieldOrdinal set to STATUS5RECORD_LOCKED
   //   will point to RowSet5.eofStatus if r4eof occurred for the row.  An r4eof is
   //     indicated by fieldOrdinal set to STATUS5EOF
   //   will point to RowSet5.goErrorStatus if d4go returned an error for the row.  A go error
   //     is indicated by fieldOrdinal set to STATUS5GO
   //   will point to a dynamically array of whatever field status values are not OK.
   //     the field ordinal specifies which field was bad.  A fieldOrdinal of STATUS5ARRAY_END
   //     indicates that there are no more elements (the array is at an end)
   //     Note that this dynamic array must be freed up when the Row5contents is freed.

   // KS Suggest simpler approach to 'status' as changed.
   Status5 *status ; // An array of status values.  Null means all OK.
}
*/

// For Row5contents only
// S4CLIENT, S4STAND_ALONE
class5 Row5memoData
{
public:
   // note that we only support transferring memo fields either as deferred or as interfaces.

   long numBytes ; // Length does not include an ending null character which
   char bytes[1] ;  // is always present at the end of 'bytes'.
} ;

/* Combined rowset functionality classes for index, rowset and schema
Note that the index functionality needs to be especially fast in the case
of just a row being accessed.
*/
class5 Row5all : public Row5buf, public Link5
{
public:
   // AS 08/27/99 --> cannot initialize in constructor because if initialization fails
   // then memory becomes corrupt (i.e. partially initialized)
   // Row5all( Rowset5all *, unsigned long recNoParm ) ;
   void initNewRow( Rowset5all *, unsigned long recNoParm ) ;
//   ~Row5all() ;
//   ~Row5() { delete using Session5.memRows }

   // refetch only the new fields (other fields may have changes).
   // convert the buffer.
   // do not free any byRef values.

   virtual void changeFieldset( Fieldset5all *newFieldset, int *iRowset ) = 0 ; //, Rowset5all *rowset ) = 0 ;
   virtual void getDeferred( Accessor5all *accessor, int iRowset ) = 0 ; //, Table5rowsetValues *rowsetValues ) = 0 ;

//   void free() ; // Called when reference count reaches zero.

   long ref ; // reference count, when 0 free structure
   Fieldset5all *fieldset ;  // pointer to the Fieldset5 defining the fields
                             // corresponding to this Row5
   virtual DBPENDINGSTATUS changedStatus() { return DBPENDINGSTATUS_UNCHANGED ; } // some row types don't allow changing - default behaviour
   int isOriginal ; // pBuffer returns the buffer from 'original' if this is true.

   DBROWSTATUS status ;
   int statusLock ; // Row level status. 0 - OK; r4locked;
   ULONG recNo ;  // 0 if it is a new record to be appended
#ifdef E4DEBUG_KEN
   void report() ;
#endif
} ;



// The contents of a row buffer.  Needed because you can have an old
// row and a current row.
// S4CLIENT, S4STAND_ALONE
// this object is a static member of Row5; therefore no special memory handling is required.
class5 Row5contents
{
public:
   // Row5contents::Row5contents() ; Note that the memory allocator will initialize to zero for speed sake.
   //   Status5list status ;

   /* the row points to all of the data corresponding to the Row.
      The Fieldset5 defines what will be contained here.
      eg. stand-alone:   [record-buffer][interface1][interface2]...[Row5memoData1]...
          client/server: [field1]...[interface1]...[Row5memoData1]... */
   void changeRowBuffer( Fieldset5all *fieldsetOld, Row5 *rowObject, char *rawBuffer, ByRef5 *byRefBuffer ) ;
   void copyOver( Row5 *row, Row5contents *from ) ;
   char *row ;

   void freeByRef( Fieldset5all *fieldset ) ;
   // AS 09/17/98 --> function called a lot, inlined and split up for eff.
   inline void free( Fieldset5all *fieldset )
   {
      row ?       /* if row != 0 */
      (
         (      /* 1st statemet */
            (fieldset->nFieldsByRef > 0 ) ?      /* if there are by ref fields */
               freeByRef( fieldset )
            :  /* else (i.e. no by ref fields) */
               0
         ),
         fieldset->memInternalRowBuffer->free( row ),   /* 2nd statment */
         row = 0  /* AS 12/09/98 3rd statement added - else was double freeing memory in some cases */
      )
      :  /* else (i.e. row == 0 ) */
         0 ;
   }
private:
   const char *getFieldContentsFromCurrentRow( Field5all *fieldInfo, int *fieldContentsLenFromRow ) ;
   void moveByRefInputToByRefField( Field5all *fieldInfoInput, char *rowInput, Field5all *fieldInfo ) ;
   void copyDataInputToByRefField( Field5all *fieldInfoInput, char *rowInput, Field5all *fieldInfo ) ;
   void copyFieldContentsInputToDataField( Field5all *fieldInfoInput, char *rowInput, Field5all *fieldInfo ) ;
   void copyFieldDataFromInputFieldSameType( Field5all *fieldInfoInput, char *rowInput, Field5all *fieldInfo ) ;
   void convertFieldDataFromInputField( Accessor5handler *accessHandler, Field5all *fieldInfoInput, const char *fieldContentsFromInputRow, int fieldContentsLenFromInputRow, Field5all *fieldInfo ) ;
   Bool5 copyFieldDataFromInputFieldIfPossible( Accessor5handler *accessHandler, Field5all *fieldInfoInput, char *rowInput, Field5all *fieldInfo ) ;
   void createFieldDataFromRawData( Accessor5handler *accessHandler, Row5 *rowObject, Field5all *fieldInfo, char *rawBuffer ) ;
   void setDataForChangedRow( Fieldset5all *fieldset, Fieldset5all *fieldsetOld, Row5 *rowObject, char *rowOld, char *rawBuffer ) ;

//   void getDeferred( unsigned long rowNo, Fieldset5 *fieldSet ) ;  See Row5::getDeferred
//   int getNonDeferred( unsigned long rowNo ) ;
//   void getDeferredDo( unsigned long rowNo, Fieldset5 *fieldSet ) ;
//   int rowObtain( unsigned long rowNo, hRow5 *row, int exposeDeleted )
//     { return Table5.rowRetrieve( rowNo, row, exposeDeleted ) ; }
} ;



/* Corresponds to an OLE DB row handle.
A given record can have more than one Row5 in existence.  ie. We
make no attempt to optimize to avoid this from happening.
S4CLIENT, S4STAND_ALONE

Data Order for Client/Server & Stand Alone:
- According to the field ordinals

Requirements:
- Fields values may change.
- New fields may get added.
- Byref retrieved memory needs to be preserved between GetData calls.

Case to consider:
- retrieve data
- set data
- retrieve data on new field
- undo
*/
class Row5oldSavedData
{
public:
   ~Row5oldSavedData() ;
   list<char *> savedRowBuffers ;
   list<Fieldset5all *> savedFieldSets ;
} ;

class5 Row5 : public Row5all
{
public:
   // AS 08/27/99 --> cannot initialize in constructor because if initialization fails
   // then memory becomes corrupt (i.e. partially initialized)
   // Row5( RowsetPlusSchema5 *, char *rawBuffer, ByRef5 *byRefBuffer, unsigned long recNoOn ) ;
   // Row5( RowsetPlusSchema5 *rowset, long recNo, int errStatus ) : Row5all(rowset, recNo) { init(rowset) ; statusLock = errStatus ; }
   // Row5( RowsetPlusSchema5 *rowset ) : Row5all(rowset, 0L ) { init(rowset) ; } // For insert
   void initNewRow( RowsetPlusSchema5 *, char *rawBuffer, ByRef5 *byRefBuffer, unsigned long recNoOn ) ;
   inline void initNewRow( RowsetPlusSchema5 *rowset, long recNo, int errStatus ) { Row5all::initNewRow( rowset, recNo) ; init( rowset ) ; statusLock = errStatus ; }
   inline void initNewRow( RowsetPlusSchema5 *rowset ) { Row5all::initNewRow( rowset, 0L ) ; init(rowset) ; } // For insert
   inline void *operator new( size_t s, Session5 *session )
   {
      // AS 09/13/99 -- the alloc() function throws Err5... if required...
      // void *p = session->memRows->alloc() ;  /* must zero out contents */
      // if ( p == 0 )
      //    throw Err5memory(0) ;
      // return p ;
      return session->memRows->alloc() ;  /* must zero out contents */
   }
   ~Row5()  ;
   void operator delete( void *p ) { ((Row5 *)p)->~Row5() ; (((Row5 *)p)->fieldset->accessorHandler->session->memRows)->free( p ) ; }

   // AS 09/22/98 delete here required in case of exception in operator new to allow
   //    freeing of memory (VC 6.0)
   void operator delete( void *p, Session5 *session ) { ((Row5 *)p)->~Row5() ; session->memRows->free( p ) ; }
   void getDeferred( Accessor5all *accessor, int iRowset ) ; //, Table5rowsetValues *rowsetValues ) = 0 ;
   void changeFieldset( Fieldset5all *newFieldset, int *iRowset ) ;

   int deleteRow() ;
   void init( RowsetPlusSchema5 *rowset ) ;
   void initToDefaults( RowsetPlusSchema5 *rowset ) ;
   inline char *pBuffer()   // AFAP
   {
      // AS 02/10/00, we no longer store the 'blank' row on an insert.  In that case, the original is NULL, but
      // we actually want to return it.  We know we need to generate it because changeStatus will be DBPENDINGSTATUS_NEW
      // return ( ( isOriginal && original.row ) ? original.row : current.row ) ;
      if ( isOriginal )
      {
         if ( original.row )
            return original.row ;
         else if ( changeStatus == DBPENDINGSTATUS_NEW )
         {
            createBlankOriginal() ;
            return original.row ;
         }
      }
      return current.row ;  // The current is the original if there is no original saved.
   }
   void createBlankOriginal() ;
   void saveToOriginal( int iRowset ) ;
   void restoreFromOriginal() { current.copyOver( this, &original ) ; }
   DBROWSTATUS undo() ;

   RowsetPlusSchema5 *rowset ;

   char deferredHere ;         // True/False; is deferred data (if any) currently present?
                               // Deferred data is kept once it is passed.
                               // maybe 3 states -> no deferred data, not here, is here
   char interfaceHere ;        // True/False; are interfaces (if any) currently setup.
                               // Data sent through an interface is not cached once
                               // it has been sent.
   Row5contents current ;
   Row5contents original ;

   // changedStatus() function required because Row5all class requires
   inline DBPENDINGSTATUS changedStatus() { return changeStatus ; }

   DBPENDINGSTATUS changeStatus ; // Has any change been made to the buffer

   void saveRowBuffer( Fieldset5all *oldFieldSet, char *rowToSave ) ;
   void freeRowBuffers() ;

   Row5oldSavedData *oldData ;   // only allocate if saving old data

   int isAlreadyDeleted ;   // flag for duplicate rows in update (conformance test change)
private:
   void copyDefaults( RowsetPlusSchema5 *rowset, char *rowBuf ) ;
} ;



// Corresponds to an OLE DB handle for index rowsets.
/* keyPtr MEMORY ALLOCATION Possibilities:
   1. keyPtr points to recNo - no keys are fetched
   2. keyPtr points to keyValue - keys are fetched with no provider memory
   3. keyPtr is allocated with alloc5() - provider memory present which was not originally present
   Row5index Memory Allocation Possibilities
   1. Allocated with session MEM4 - originally just recNo needed.
   2. Allocated dynamically - originally more than just the recNo.
*/
class5 Row5index : public Row5all
{
public:
   Rowset5index *rowIndex ;
//    unsigned long rowNo ;  -- use Recno from Row5all
//KS int isKey ;
// Compile Test Change    Row5index( Rowset5index *iSet, int hasKey = 0 ) ;

   void init( Rowset5index *rwsIndex ) ;

   void getDeferred( Accessor5all *accessor, int iRowset ) { ; } // not relevant
   // AS 08/27/99 --> cannot initialize in constructor because if initialization fails
   // then memory becomes corrupt (i.e. partially initialized)
   // Row5index( Rowset5index *rwsIndex ) : Row5all( rwsIndex, 0 ) { init( rwsIndex ) ; }
   void initNewRow( Rowset5index *rwsIndex ) { Row5all::initNewRow( rwsIndex, 0 ) ; init( rwsIndex ) ; }
   void operator delete( void *p ) ;
   void *operator new( size_t s, Rowset5index *rowset ) ;// ...Session5.memRowIndexNoKey  Lets
   // AS 09/22/98 delete here required in case of exception in operator new to allow
   //    freeing of memory (VC 6.0)
   void operator delete( void *p, Rowset5index *rowset ) { delete( p ) ; }
   ~Row5index() ; // if key allocated dynamically or not here, use Session5.memRowIndexNoKey for freeing
   int nullTest( int nullBitPosition ) ;

   void getHandle( int direction ) ;
   void refetchWithKey() ;
   int isSendRecNo() { return keyPtr == (char *) &recNo ; }

   void changeFieldset( Fieldset5all *newFieldset, int *iRowset ) ; //, Rowset5all *rowset ) ;
   inline char *pBuffer() { return keyPtr ; }
   char isDynamicFree ;
   int status ;  // if fetch failed (eg. r4locked)
//KS   int isKeyInit ;  // when we decide to get the key - has rawToRow, etc. been inited

   char *keyPtr ;   // in case it is a dynamic allocation (i.e. if keyPtr != &keyValue )

   // keyValue must be at end of class because it is an allocated structure that fits into memory
   char keyValue[1] ; // Memory amount specified at runtime for this member
                     // Decompress as keyValue's get filled in.
} ;
#endif /* S4SERVER */

#endif /* OLEDB5BUILD */
