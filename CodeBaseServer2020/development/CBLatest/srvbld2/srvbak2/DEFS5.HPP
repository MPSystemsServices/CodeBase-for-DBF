// defs5.hpp

#define NUM5CACHED_TABLES 20

// AS 09/22/99 - previous values 50/25 too small for larger rowsets...
#define MEM5START_ROW  250
#define MEM5EXPAND_ROW 5000

// large enough to contain L"CHARACTER_MAXIMUM_LENGTH"
#define LEN5COLUMN_NAME 50

/* TABLE_TYPE big enough to contain "GLOABLY TEMPORARY", in unicode */
#define LEN5I_TYPE          2
#define LEN5TABLE_TYPE     32
/* we want the table-catalog to be as large as possible...
   restrictions are:
    max key size 240
    expressions are:
       CATALOG + TBL_NAME(32) + COL_NAME(40) : max = 168
       CATALOG + TBL_NAME(32) + IDX_NAME(22) : max = 186
       CATALOG + TBL_NAME(32) + TBL_TYPE(32) : max = 176

       therefore set to 168
*/
#define LEN5TABLE_CATALOG 158
#define LEN5TABLE_NAME     32
#define LEN5NULL            1
#define LEN5DESCRIPTION   100

#define REQUEST5SEQUENTIAL  0
#define REQUEST5ARRAY       1
#define REQUEST5SINGLE      2

#define RANGE5MATCH   1
#define RANGE5PREFIX  2
#define RANGE5NORMAL  3

/* these next ones are orred together, so must be differring bits */
#define r5prefix 0x01
#define r5greaterEndKey 0x02
#define r5lessStartKey 0x04
#define r5match 0x08
#define r5null 0x10

#define ROWSET5START_ROW  25
#define ROWSET5EXPAND_ROW 25

#define createFieldSequential5 1
#define offset5bookmark 0

#define STATUS5READING 0
#define STATUS5WRITING 1
#define STATUS5NOTHING 2

// must be quite large to handle full-width schema field names
#define FIELD5NAME_MAX_BYTES 54

#define STATUS5ARRAY_END     -2
#define STATUS5RECORD_LOCKED -3
#define STATUS5GO            -4
#define STATUS5DELETED       -5

#define VAR5FALSE VARIANT_FALSE  /* 0x00*/
#define VAR5TRUE  VARIANT_TRUE   /* 0xffff */

#define PROP5INFODESC_BLOCKSIZE        1024

#define BLAST5NUM_BUFFERS 10

#define BLAST5FILE_BUFFER_SIZE 16384

#define STATUS5READINGING 0
#define STATUS5WRITING 1
#define STATUS5NOTHING 2

#define TYPE5SCHEMA  1
#define TYPE5CLIENT  2
#define TYPE5STAND   3
#define TYPE5ANY    10

#define LEN5TAG_NAME 22
#define LEN5I4            4
#define LEN5FILTER_COND   256

#define LEN5MAX_DOUBLE_WIDTH 323

/*
- replaced by an enum
#define SCHEMA5DIRECT        0
#define SCHEMA5OPTIMIZATION  1
#define SCHEMA5DONE          2
#define SCHEMA5EOF           3
*/

// the max # of rows to return on a single call to GetNextRows()
#define ROWSET5MAX_GET_HANDLES  10000

// below == # of wide characters
#define LEN5MAX_TYPE_NAME         20
//#define LEN5MAX_LITERAL_PREFIX     1
//#define LEN5MAX_LITERAL_SUFFIX     0
#define LEN5MAX_CREATE_PARAMS     50
#define LEN5MAX_LOCAL_TYPE_NAME   20
#define LEN5MAX_TYPE_LIB          20
#define LEN5MAX_VERSION           20

#define i5str     r4str
#define i5bool    r4log
#define i5bytes   r4charBin

/* use values before alpha-numeric which are used for most types */

/* changed to using type5, so don't bother...
#define i5wstrDesc       1
#define i5strDesc        2
#define i5dateTimeDesc   3
#define i5intDesc        4
#define i5currencyDesc   5
#define i5dateTimeAsc    6
#define i5intAsc         7
#define i5currencyAsc    8
#define i5double         9
*/

// NO5TRANSACTION must not conflict with any of THE ISOLATIONLEVEL ole-db settings (in its enumerator)
#define NO5TRANSACTION 0

#define SCHEMA5OLD -1
#define SCHEMA5READING_DONE 1

// purpose of Err5client() is for easy text searching as to where the
// breakdowns are in the CodeBase client for ole-db
#define Err5client() Err5internal(0)

#define SCHEMA5ID_TABLES  -1
#define SCHEMA5ID_INDEXES -2
#define SCHEMA5ID_COLUMNS -3

#define LIT5NULL L""

// AS 12/05/00 - since indepenedent dll, must always reset valiue...
//#ifdef S4CLIENT
   #define F5SET_NUMERIC -1
//#else
//   #define F5SET_NUMERIC F4MAX_NUMERIC
//#endif

#ifndef S4SERVER
   #define PROVS5NULL_BYTE_OFFSET ( sizeof(Provider5typeTable) - 2 * sizeof( char ) - sizeof( long ) )
#endif


#ifdef OLEDB5ERROR
   #define S5EXPORT
#endif

#define ERR_CODEBASE_CREATE         60    /* match similar r4 return codes - r4noCreate */
#define ERR_CODEBASE_CREATEINDEX    61    /* match similar r4 return codes - r4noCreate  for Clipper Indexes */
#define ERR_CODEBASE_CREATEACCESSOR 200   /* true OLE DB errors */
#define ERR_CODEBASE_CONVERSION     210
#define ERR_CODEBASE_DELETE         220
#define ERR_CODEBASE_PROPERTIES     230
#define ERR_CODEBASE_BADNAME        1440   /* e4name error */
#define ERR_CODEBASE_TRANSACTION    1210   /* e4trans error */
