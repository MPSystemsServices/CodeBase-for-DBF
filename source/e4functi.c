/* *********************************************************************************************** */
/* Copyright (C) 1999-2015 by Sequiter, Inc., 9644-54 Ave, NW, Suite 209, Edmonton, Alberta Canada.*/
/* This program is free software: you can redistribute it and/or modify it under the terms of      */
/* the GNU Lesser General Public License as published by the Free Software Foundation, version     */
/* 3 of the License.                                                                               */
/*                                                                                                 */
/* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;       */
/* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.       */
/* See the GNU Lesser General Public License for more details.                                     */
/*                                                                                                 */
/* You should have received a copy of the GNU Lesser General Public License along with this        */
/* program. If not, see <https://www.gnu.org/licenses/>.                                           */
/* *********************************************************************************************** */

/* e4functi.c  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

#include "d4all.h"
#ifndef S4UNIX
#ifdef __TURBOC__
   #pragma hdrstop
#endif
#endif

#if !defined( S4NO_POW ) && !defined( S4PALM )
   #include <math.h>
#endif

#ifndef S4OFF_REPORT
   double S4FUNCTION total4value( TOTAL4 *t4 );
#endif

const E4FUNCTIONS v4functions[EXPR4NUM_FUNCTIONS] =
{
   /* function, name, code, nameLen, priority, returnType, numParms, type[]*/
   { e4fieldAdd,   0,   0, 0, 0, r4str,      0, 0, 0, 0 },  /*  1 - E4FIELD_STR */
   { e4fieldAdd,   0,   0, 0, 0, r5wstr,     0, 0, 0, 0 },  /*  2 - E4FIELD_WSTR */
   { e4fieldCopy,  0,   0, 0, 0, r5wstrLen,  0, 0, 0, 0 },  /*  3 - E4FIELD_WSTR_LEN */
   { e4fieldCopy,  0,   1, 0, 0, r4str,      0, 0, 0, 0 },  /*  4 - E4FIELD_STR_CAT */
   { e4fieldCopy,  0,   2, 0, 0, r5wstr,     0, 0, 0, 0 },  /*  5 - E4FIELD_STR_CAT */
   { e4fieldLog,   0,   3, 0, 0, r4log,      0, 0, 0, 0 },  /*  6 - E4FIELD_LOG */
   { e4fieldDateD, 0,   4, 0, 0, r4dateDoub, 0, 0, 0, 0 },  /*  7 - E4FIELD_DATE_D */
   { e4fieldAdd,   0,   5, 0, 0, r4date,     0, 0, 0, 0 },  /*  8 - E4FIELD_DATE_S */
   { e4fieldNumD,  0,   6, 0, 0, r4numDoub,  0, 0, 0, 0 },  /*  9 - E4FIELD_NUM_D */
   { e4fieldAdd,   0,   7, 0, 0, r4num,      0, 0, 0, 0 },  /* 10 - E4FIELD_NUM_S */
   { e4fieldAdd,   0, 440, 0, 0, r4currency, 0, 0, 0, 0 },  /* 11 - E4FIELD_CUR */
   // AS July 27/01 - field value is stored in intel ordering always, so need a special function to extract
   // the value in local double ordering format
   { e4fieldDoubD, 0, 441, 0, 0, r4numDoub,  0, 0, 0, 0 },  /* 12 - E4FIELD_DOUB */
   { e4fieldAdd,   0, 442, 0, 0, r4int,      0, 0, 0, 0 },  /* 13 - E4FIELD_INT */
   { e4fieldAdd,   0, 443, 0, 0, r5ui4,      0, 0, 0, 0 },  /* 14 - E4FIELD_UNS_INT */
   { e4fieldAdd,   0, 444, 0, 0, r5i2,       0, 0, 0, 0 },  /* 15 - E4FIELD_SHORT */
   { e4fieldAdd,   0, 445, 0, 0, r5ui2,      0, 0, 0, 0 },  /* 16 - E4FIELD_UNS_SHORT */
   { e4fieldAdd,   0, 446, 0, 0, r4dateTime, 0, 0, 0, 0 },  /* 17 - E4FIELD_DTTIME */
   { e4fieldAdd,   0, 447, 0, 0, r4dateTimeMilli, 0, 0, 0, 0 },  /*  18- E4FIELD_DTTIME_MILLI */   // AS Mar 10/03 - ms support in datetime
   { e4fieldIntD,  0, 448, 0, 0, r4numDoub,  0, 0, 0, 0 },  /* 19 - E4FIELD_INT_D */
   { e4fieldCurD,  0, 449, 0, 0, r4numDoub,  0, 0, 0, 0 },  /* 20 - E4FIELD_CUR_D */
   { e4fieldAdd,   0, 450, 0, 0, r5i8,       0, 0, 0, 0 },  /* 21 - E4FIELD_I8 */
   { e4fieldAdd,   0, 451, 0, 0, r5dbDate,   0, 0, 0, 0 },  /* 22 - E4FIELD_DBDATE */
   { e4fieldAdd,   0, 452, 0, 0, r5dbTime,   0, 0, 0, 0 },  /* 23 - E4FIELD_DBTIME */
   { e4fieldAdd,   0, 453, 0, 0, r5dbTimeStamp, 0, 0, 0, 0 }, /* 24 - E4FIELD_DBTIMESTAMP */
   // AS Jul 21/05 - Support for new field type binary float
   { e4fieldAdd,   0, 454, 0, 0, r4floatBin, 0, 0, 0, 0 }, /* 25 - E4FIELD_BINFLOAT */
   // { e4fieldFloatD,   0, 455, 0, 0, r4numDoub, 0, 0, 0, 0 }, /* 26 - E4FIELD_BINFLOAT_D */
   // { e4fieldFloatD,   0, 455, 0, 0, r5i8, 0, 0, 0, 0 }, /* 26 - E4FIELD_BINFLOAT_D  -  a dummy holder for now */
   { e4fieldIntF,  0, 456, 0, 0, r4float,    0, 0, 0, 0 },  /* 26 - E4FIELD_INT_F */

   /* *** E4LAST_FIELD IS SET TO SAME VALUE AS E4FIELD_MEMO,
          THIS MEANS ALL NEW FIELD ADDITIONS MUST GO BEFORE THIS COMMENT LINE
   */
   #ifdef S4OFF_MEMO
      { 0,           0, 7, 0,  0, r4str, 0, 0, 0, 0 },       /* 27- */
   #else
      { e4fieldMemo, 0, 7, 0,  0, r4str, 0, 0, 0, 0 },       /* 27- E4FIELD_MEMO */
   #endif

   { e4copyConstant, 0,  8, 0, 0, r4numDoub,  0, 0, 0, 0 },   /* 28 - E4DOUBLE */
   { e4copyConstant, 0,  9, 0, 0, r4str,      0, 0, 0, 0 },   /* 29 - E4STRING */
   { e4copyToFloat, 0, 10, 0, 0, r4floatBin,  0, 0, 0, 0 },   /* 30 - E4FLOAT */

   /* E4FIRST_LOG STARTS AT E4LAST_FIELD + 4 (i.e. after copy constants) */
   { expr4trueFunction, ".TRUE.",  14, 6, 0, r4log, 0,     0, 0, 0 }, /* 31 - */
   { expr4trueFunction, ".T.",     14, 3, 0, r4log, 0,     0, 0, 0 }, /* 32 - */
   { e4false,           ".FALSE.", 16, 7, 0, r4log, 0,     0, 0, 0 }, /* 33 - */
   { e4false,           ".F.",     16, 3, 0, r4log, 0,     0, 0, 0 }, /* 34 - */
   { e4not,             ".NOT.",   18, 5, 5, r4log, 1, r4log, 0, 0 }, /* 35 - */
   { e4not,             "!",   18, 1, 5, r4log, 1, r4log, 0, 0 },     /* 36 - */

   /* E4LAST_LOG IS SET AT E4FIRST_LOG + 4 (since inclusive 10 to 14 = 5) */

   /* E4FIRST_OPERATOR IS SET AT E4LAST_LOG + 1 */
   /* E4LAST_OPERATOR IS SET AT E4FIRST_OPERATOR + 58 (59 entries less one for exclusive) */
   { e4or,            ".OR.",   20, 4, 3, r4log, -1, r4log, 0, 0 }, /*  37 - Flexible # of parms.*/
   { e4and,           ".AND.",  22, 5, 4, r4log, -1, r4log, 0, 0 }, /*  38 - */

   { e4parmRemove,   "+", 25, 1, 7, r4str,      2, r4str,     r4str,      0 },  /*  39 - Concatenate */
   { e4parmRemove,     0, 25, 0, 7, r5wstr,     2, r5wstr,    r5wstr,     0 },  /*  40 - Concatenate */
   { e4wstrLenCon,     0, 25, 0, 7, r5wstrLen,  2, r5wstrLen, r5wstrLen,     0 },  /*  41 - Concatenate */
   { e4concatTrim,     0, 25, 0, 7, r4str,      2, r4str,     r4str,      0 },  /*  42 - Concatenate */
   { e4add,            0, 25, 0, 7, r4numDoub,  2, r4numDoub, r4numDoub,  0 }, /*  43 - */
   { e4addDate,        0, 25, 0, 7, r4dateDoub, 2, r4numDoub, r4dateDoub, 0 }, /*  44 - */
   { e4addDate,        0, 25, 0, 7, r4dateDoub, 2, r4dateDoub,r4numDoub,  0 }, /*  45 - */
   { e4addFloat,       0, 25, 0, 7, r4floatBin,  2, r4floatBin, r4floatBin,  0 }, /*  46 - */

   { e4concatTwo,    "-", 30, 1, 7, r4str,      2, r4str,      r4str,      0 }, /*  47 - */
   { e4sub,            0, 30, 0, 7, r4numDoub,  2, r4numDoub,  r4numDoub,  0 }, /*  48 - */
   { e4subDate,        0, 30, 0, 7, r4numDoub,  2, r4dateDoub, r4dateDoub, 0 }, /*  49 - */
   { e4subDate,        0, 30, 0, 7, r4dateDoub, 2, r4dateDoub, r4numDoub,  0 }, /*  50 - */
   { e4subFloat,       0, 30, 0, 7, r4floatBin,  2, r4floatBin,  r4floatBin,  0 }, /*  51 - */

   /* E4COMPARE_START IS E4FIRST_OPEATOR + 12 */
   { e4notEqual,       "#",  50, 1, 6, r4log, 2, r4str,      r4str,      0 }, /*  52 - */
   { e4notEqual,      "<>",  50, 2, 6, r4log, 2, r4str,      r4str,      0 }, /*  53 - */
   { e4notEqual,         0,  50, 0, 6, r4log, 2, r4numDoub,  r4numDoub,  0 }, /*  54 - */
   { e4notEqual,         0,  50, 0, 6, r4log, 2, r4dateDoub, r4dateDoub, 0 }, /*  55 - */
   { e4notEqual,         0,  50, 0, 6, r4log, 2, r4log,      r4log,      0 }, /*  56 - */
   { e4notEqualCur,      0,  50, 0, 6, r4log, 2, r4currency, r4currency, 0 }, /*  57 - */
   { e4notEqualDtTime,   0,  50, 0, 6, r4log, 2, r4dateTime, r4dateTime, 0 }, /*  58 - */
   { e4notEqualDtTime,   0,  50, 0, 6, r4log, 2, r4dateTimeMilli, r4dateTimeMilli, 0 }, /*  59 - */
   { e4notEqual,         0,  50, 0, 6, r4log, 2, r4floatBin,  r4floatBin,  0 }, /*  60 - */

   { e4greaterEq,     ">=",  60, 2, 6, r4log, 2, r4str,      r4str,      0 }, /*  61 - */
   { e4greaterEq,     "=>",  60, 2, 6, r4log, 2, r4str,      r4str,      0 }, /*  62 - */
   { e4greaterEqDoub,    0,  60, 0, 6, r4log, 2, r4numDoub,  r4numDoub,  0 }, /*  63 - */
   { e4greaterEqDoub,    0,  60, 0, 6, r4log, 2, r4dateDoub, r4dateDoub, 0 }, /*  64 - */
   { e4greaterEqCur,     0,  60, 0, 6, r4log, 2, r4currency, r4currency, 0 }, /*  65 - */
   { e4greaterEqDtTime,  0,  60, 0, 6, r4log, 2, r4dateTime, r4dateTime, 0 }, /*  66 - */
   { e4greaterEqDtTime,  0,  60, 0, 6, r4log, 2, r4dateTimeMilli, r4dateTimeMilli, 0 }, /*  67 - */
   { e4greaterEqFloat,   0,  60, 0, 6, r4log, 2, r4floatBin, r4floatBin, 0 }, /*  68 - */

   { e4lessEq,        "<=",  70, 2, 6, r4log, 2, r4str,      r4str, 0 },        /*  69 - */
   { e4lessEq,        "=<",  70, 2, 6, r4log, 2, r4str,      r4str, 0 },        /*  70 - */
   { e4lessEqDoub,       0,  70, 0, 6, r4log, 2, r4numDoub,  r4numDoub, 0 },    /*  71 - */
   { e4lessEqDoub,       0,  70, 0, 6, r4log, 2, r4dateDoub, r4dateDoub, 0 },   /*  72 - */
   { e4lessEqCur,        0,  70, 0, 6, r4log, 2, r4currency, r4currency, 0 },   /*  73 - */
   { e4lessEqDtTime,     0,  70, 0, 6, r4log, 2, r4dateTime, r4dateTime, 0 },   /*  74 - */
   { e4lessEqDtTime,     0,  70, 0, 6, r4log, 2, r4dateTimeMilli, r4dateTimeMilli, 0 },   /*  75 - */
   { e4lessEqFloat,      0,  70, 0, 6, r4log, 2, r4floatBin, r4floatBin, 0 },   /*  76 - */

   { e4equal,          "=",  40, 1, 6, r4log, 2, r4str,      r4str,      0 },   /*  77 - */
   { e4equal,            0,  40, 0, 6, r4log, 2, r4log,      r4log,      0 },   /*  78 - */
   { e4equal,            0,  40, 0, 6, r4log, 2, r4numDoub,  r4numDoub,  0 },   /*  79 - */
   { e4equal,            0,  40, 0, 6, r4log, 2, r4dateDoub, r4dateDoub, 0 },   /*  80 - */
   { e4equalCur,         0,  40, 0, 6, r4log, 2, r4currency, r4currency, 0 },   /*  81 - */
   { e4equalDtTime,      0,  40, 0, 6, r4log, 2, r4dateTime, r4dateTime, 0 },   /*  82 - */
   { e4equalDtTime,      0,  40, 0, 6, r4log, 2, r4dateTimeMilli, r4dateTimeMilli, 0 }, /*  83 - */
   { e4equal,            0,  40, 0, 6, r4log, 2, r4floatBin, r4floatBin, 0 }, /*  84 - */

   { e4greater,          ">",  80, 1, 6, r4log, 2, r4str, r4str, 0 },           /*  85 - */
   { e4greaterDoub,       0,  80, 0, 6, r4log, 2, r4numDoub,  r4numDoub, 0 },   /*  86 - */
   { e4greaterDoub,       0,  80, 0, 6, r4log, 2, r4dateDoub, r4dateDoub, 0 },  /*  87 - */
   { e4greaterCur,        0,  80, 0, 6, r4log, 2, r4currency, r4currency, 0 },  /*  88 - */
   { e4greaterDtTime,     0,  80, 0, 6, r4log, 2, r4dateTime, r4dateTime, 0 },  /*  89 - */
   { e4greaterDtTime,     0,  80, 0, 6, r4log, 2, r4dateTimeMilli, r4dateTimeMilli, 0 },  /*  90 - */
   { e4greaterFloat,     0,  80, 0, 6, r4log, 2, r4floatBin, r4floatBin, 0 },  /*  91 - */

   { e4less,             "<",  90, 1, 6, r4log, 2, r4str, r4str, 0 },           /*  92 - */
   { e4lessDoub,          0,  90, 0, 6, r4log, 2, r4numDoub,  r4numDoub, 0 },   /*  93 - */
   { e4lessDoub,          0,  90, 0, 6, r4log, 2, r4dateDoub, r4dateDoub, 0 },  /*  94 - */
   { e4lessCur,           0,  90, 0, 6, r4log, 2, r4currency, r4currency, 0 },  /*  95 - */
   { e4lessDtTime,        0,  90, 0, 6, r4log, 2, r4dateTime, r4dateTime, 0 },  /*  96 - */
   { e4lessDtTime,        0,  90, 0, 6, r4log, 2, r4dateTimeMilli, r4dateTimeMilli, 0 },  /*  97 - */
   { e4lessFloat,        0,  90, 0, 6, r4log, 2, r4floatBin, r4floatBin, 0 },  /*  98 - */
   /* E4COMPARE_END IS E4COMPARE_START + 41 */

   #ifdef S4NO_POW
      {       0,             0,   95, 0, 0, r4numDoub, 2, r4numDoub, 0, 0 },    /*  99 - */
      {       0,             0,   95, 0, 0, r4numDoub, 2, r4numDoub, 0, 0 },    /* 100 - */
   #else
      { e4power,           "^",  100, 1, 9, r4numDoub, 2, r4numDoub, r4numDoub, 0}, /*  99 - */
      { e4power,          "**",  100, 2, 9, r4numDoub, 2, r4numDoub, r4numDoub, 0}, /* 100 - */
   #endif

   { e4multiply,         "*", 102, 1, 8, r4numDoub, 2, r4numDoub, r4numDoub, 0},  /* 101 - */
   { e4multiplyFlt,      "*", 102, 1, 8, r4floatBin, 2, r4floatBin, r4floatBin, 0},  /* 102 - */
   { e4divide,           "/", 105, 1, 8, r4numDoub, 2, r4numDoub, r4numDoub, 0},  /* 103 - */
   { e4divideFlt,        "/", 105, 1, 8, r4floatBin, 2, r4floatBin, r4floatBin, 0},  /* 104 - */
   { e4contain,          "$", 110, 1, 6, r4log, 2, r4str, r4str, 0 },  /* 105 - */

   /* E4FIRST_FUNCTION IS E4COMPARE_END + 6 --> since compare_end was
      inclusive, should be 1 more than above functions (which is 5) */
   { e4chr,       "CHR", 120, 3, 0, r4str, 1, r4numDoub,  0, 0 },   /* 106 - */
   { e4del,       "DEL", 130, 3, 0, r4str, 0, 0,          0, 0 },   /* 107 - */
   { e4str,       "STR", 140, 3, 0, r4str, 1, r4numDoub,  0, 0 },   /* 108 - */
   { e4wideToStr,       "STR", 140, 1, 0, r4str, 1, r5wstr,  0, 0 }, /* 109 - */
   // As 07/16/99 --> Added new function ... StrZero - similar as Str but adds 0's on the left...
   { e4strZero,   "STRZERO", 145, 7, 0, r4str, 1, r4numDoub,  0, 0 },  /* 110 - */
   { e4substr, "SUBSTR", 150, 6, 0, r4str, 1, r4str,      0, 0 },      /* 111 - */
   { e4time,     "TIME", 160, 4, 0, r4str, 0, 0,          0, 0 },      /* 112 - */
   { e4upper,   "UPPER", 170, 5, 0, r4str, 1, r4str,      0, 0 },      /* 113 - */
   { e4copyParm, "DTOS", 180, 4, 0, r4str, 1, r4date,     0, 0 },      /* 114 - */
   { e4dtosDoub,      0, 180, 0, 0, r4str, 1, r4dateDoub, 0, 0 },      /* 115 - */
   { e4dtoc,     "DTOC", 200, 4, 0, r4str, 1, r4date,     0, 0 },      /* 116 - */
   { e4dtocDoub,      0, 200, 4, 0, r4str, 1, r4dateDoub, 0, 0 },      /* 117 - */
   // AS Apr 22/04 - support for TTOC
   { e4ttoc,     "TTOC", 205, 4, 0, r4str, 1, r4date, 0, 0 },  /* 118 - */
   { e4ttocDateTime,     0, 205, 4, 0, r4str, 1, r4dateTime,  0, 0 },  /* 119 - */
   { e4ttocDoub, 0, 205, 4, 0, r4str, 1, r4dateDoub,  0, 0 },  /* 120 - */

   // AS July 25/02 - Added new function SPACE(N)
   { e4space,    "SPACE",210, 5, 0, r4str, 1, r4numDoub, 0, 0 },       /* 121 - */


   { e4trim,     "TRIM",220, 4, 0, r4str, 1, r4str, 0, 0 },           /* 122 - */
   { e4ltrim,   "LTRIM",230, 5, 0, r4str, 1, r4str, 0, 0 },           /* 123 - */
   { e4alltrim, "ALLTRIM",235, 7, 0, r4str, 1, r4str, 0, 0 },         /* 124 - */
   { e4substr,   "LEFT",240, 4, 0, r4str, 1, r4str, 0, 0 },           /* 125 - */
   { e4substr,   "RIGHT",245, 5, 0, r4str, 1, r4str, 0, 0 },          /* 126 - */

   { e4padLeft,  "PADL", 246, 4, 0, r4str, 1, r4str, 0, 0 },          /* 127 - */
   { e4padRight, "PADR", 247, 4, 0, r4str, 1, r4str, 0, 0 },          /* 128 - */

   { e4iifStr,  "IIF", 250, 3, 0,      r4str, 3, r4log,      r4str, r4str },  /* 129 - */
   { e4iif,      0, 250, 0, 0,  r4numDoub, 3, r4log,  r4numDoub, r4numDoub},  /* 130 - */
   { e4iif,      0, 250, 0, 0, r4dateDoub, 3, r4log, r4dateDoub, r4dateDoub}, /* 131 - */
   { e4iif,      0, 250, 0, 0,      r4log, 3, r4log,      r4log, r4log },     /* 132 - */

   // 09/20/00 - DATETIME() function
   // AS Mar 10/03 - ms support in datetime - support optional millisecond paramater
   { e4dateTime,    "DATETIME", 259, 8, 0, r4dateTime, 3, r4numDoub,r4numDoub,r4numDoub },  /* 133 - */
   { e4dateTime,    0, 259, 8, 0, r4dateTimeMilli, 3, r4numDoub,r4numDoub,r4numDoub },  /* 134 - */
   { e4stod,        "STOD",  260, 4, 0, r4dateDoub, 1,      r4str, 0, 0 },                  /* 135 - */
   { e4ctod,        "CTOD",  270, 4, 0, r4dateDoub, 1,      r4str, 0, 0 },                  /* 136 - */
   { e4date,        "DATE",  280, 4, 0, r4dateDoub, 0,          0, 0, 0 },                  /* 137 - */
   { e4day,          "DAY",  290, 3, 0,  r4numDoub, 1,     r4date, 0, 0 },                  /* 138 - */
   { e4dayDoub,          0,  290, 0, 0,  r4numDoub, 1, r4dateDoub, 0, 0 },                  /* 139 - */
   { e4month,      "MONTH",  310, 5, 0,  r4numDoub, 1,     r4date, 0, 0 },                  /* 140 - */
   { e4monthDoub,        0,  310, 0, 0,  r4numDoub, 1, r4dateDoub, 0, 0 },                  /* 141 - */
   { e4year,        "YEAR",  340, 4, 0,  r4numDoub, 1,     r4date, 0, 0 },                  /* 142 - */
   { e4yearDoub,         0,  340, 0, 0,  r4numDoub, 1, r4dateDoub, 0, 0 },                  /* 143 - */
   { e4deleted,  "DELETED",  350, 7, 0,      r4log, 0,          0, 0, 0 },                  /* 144 - */
//   { e4notDeleted,  "!DELETED",  355, 8, 0,  r4log, 0,          0, 0, 0 },
   { e4recCount,"RECCOUNT",  360, 8, 0,  r4numDoub, 0,          0, 0, 0 },                  /* 145 - */
   { e4recno,      "RECNO",  370, 5, 0,  r4numDoub, 0,          0, 0, 0 },                  /* 146 - */
   { e4val,          "VAL",  380, 3, 0,  r4numDoub, 1,      r4str, 0, 0 },                  /* 147 - */

   #ifndef S4UNIX
      { e4l2bin,      "L2BIN",  385, 5, 0,      r4str, 1,  r4numDoub, 0, 0 },               /* 148 - */
   #else
      { 0,                  0,  385, 5, 0,      r4str, 1,  r4numDoub, 0, 0 },               /* 148 - */
   #endif

   { e4calcFunction,     0,  390, 0, 0,          0, 0,          0, 0, 0 },                  /* 149 - */

   #ifndef S4OFF_REPORT
      /* { e4calcFunction,     0,  390, 0, 0,          0, 0,          0, 0, 0 },*/
      { e4calcTotal,        0,  400, 0, 0,  r4numDoub, 0,          0, 0, 0 },               /* 150 - */
      { e4pageno,    "PAGENO",  410, 6, 0,  r4numDoub, 0,          0, 0, 0 },               /* 151 - */
   #else
      /* { 0,                  0,  390, 0, 0,          0, 0,          0, 0, 0 },*/
      { 0,                  0,  400, 0, 0,  r4numDoub, 0,          0, 0, 0 },               /* 150 - */
      { 0,                  0,  410, 6, 0,  r4numDoub, 0,          0, 0, 0 },               /* 151 - */
   #endif

   // AS Jun 27/03 - Added EMPTY() function - for now for r4str types only
   { e4empty,         "EMPTY",  415, 5, 0,      r4log, 1,        r4str, 0, 0 },             /* 152 - */
   { e4emptyNum,            0,  415, 5, 0,      r4log, 1,        r4numDoub, 0, 0 },         /* 153 - */
   { e4emptyDtTm,           0,  415, 5, 0,      r4log, 1,        r4dateTime, 0, 0 },        /* 154 - */
   { e4emptyNum,           0,   415, 5, 0,      r4log, 1,        r4dateDoub, 0, 0 },        /* 155 - */
   { e4emptyLog,           0,   415, 5, 0,      r4log, 1,        r4log, 0, 0 },             /* 156 - */

   /* DESCEND() only works like Clipper with Char parameter. */
   { e4descend,    "DESCEND",  420, 7, 0, r4str, 1, r4str,     0, 0 },                      /* 157 - E4DESCEND_STR */
   { e4descend,            0,  420, 7, 0, r4str, 1, r4num,     0, 0 },                      /* 158 - E4DESCEND_NUM */
   { e4descend,            0,  420, 7, 0, r4str, 1, r4dateDoub,0, 0 },                      /* 159 - E4DESCEND_DATE_D */
   { e4descend,            0,  420, 7, 0, r4str, 1, r4log,     0, 0 },                      /* 160 - E4DESCEND_LOG */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r4dateTime, 0, 0 },                     /* 161 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r4int, 0, 0 },                          /* 162 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r4currency, 0, 0 },                     /* 163 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5wstr, 0, 0 },                         /* 164 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5wstrLen, 0, 0 },                      /* 165 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r4numDoub, 0, 0 },                      /* 166 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5i8, 0, 0 },                           /* 167 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5ui4, 0, 0 },                          /* 168 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5ui2, 0, 0 },                          /* 169 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5i2, 0, 0 },                           /* 170 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5dbDate, 0, 0 },                       /* 171 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5dbTime, 0, 0 },                       /* 172 - */
   { e4descendBinary,      0,  420, 7, 0, r4str, 1, r5dbTimeStamp, 0, 0 },                  /* 173 - */
   { e4ascend,      "ASCEND",  430, 6, 0, r4str, 1, r4str,     0, 0 },                      /* 174 - E4ASCEND_STR */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5wstr,    0, 0 },                      /* 175 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5wstrLen, 0, 0 },                      /* 176 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4num,     0, 0 },                      /* 177 - E4ASCEND_NUM */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4date,    0, 0 },                      /* 178 - E4ASCEND_DATE */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4dateDoub,0, 0 },                      /* 179 - E4ASCEND_DATE_D */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4log,     0, 0 },                      /* 180 - E4ASCEND_LOG */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4int,    0, 0 },                       /* 181 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4dateTime, 0, 0 },                     /* 182 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4currency,    0, 0 },                  /* 183 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r4numDoub, 0, 0 },                      /* 184 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5ui4, 0, 0 },                          /* 185 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5ui2, 0, 0 },                          /* 186 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5i2, 0, 0 },                           /* 187 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5i8, 0, 0 },                           /* 188 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5dbDate, 0, 0 },                       /* 189 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5dbTime, 0, 0 },                       /* 190 - */
   { e4ascend,             0,  430, 6, 0, r4str, 1, r5dbTimeStamp, 0, 0 },                  /* 191 - */

   // AS Jan 6/05 - Added support for a new KEYEQUAL function (useful with general collations for example to make a-accent == a for partial match)
   { e4keyEqual, "KEYEQUAL",  440, 8, 0, r4log, 2, r4str, r4str, 0 },                  /* 192 - */
   { 0,0, E4TERMINATOR, 0, 0, 0, 0, 0, 0, 0 },
} ;



/* The following function was added so that the OLEDB dll will have access to the above data.*/
const E4FUNCTIONS *S4FUNCTION e4functions()
{
  return (&v4functions[0]);
}


// AS Dec 16/04 - support for variable trimming to store the resulting length
#define expr4infoPtrLen( e4 ) ( ((e4)->varLength) ? ((e4)->i2) : ((e4)->len) )


void S4FUNCTION expr4functions( const E4FUNCTIONS **fptr )
{
   *fptr = v4functions ;
}



void e4add()
{
   #ifdef S4DATA_ALIG2
      double a,b,c;
   #endif
   double *doublePtr = (double *) (expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *doublePtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            memcpy(&a, expr4[-2], sizeof(double));
            memcpy(&b, expr4[-1], sizeof(double));
            c = a + b;
            memcpy(doublePtr, &c, sizeof(double));
         #else
            *doublePtr = *(double *)expr4[-2] + *(double *)expr4[-1] ;
         #endif
      }

   expr4[-2] = (char *) doublePtr ;
   expr4-- ;
}



void e4addFloat()
{
   #ifdef S4DATA_ALIG2
      double a,b,c;
   #endif
   float *floatPtr = (float *) (expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *floatPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            assert5( 0 ) ;  // not supported
            // memcpy(&a, expr4[-2], sizeof(float));
            // memcpy(&b, expr4[-1], sizeof(float));
            // c = a + b;
            // memcpy(floatPtr, &c, sizeof(float));
         #else
            *floatPtr = *(float *)expr4[-2] + *(float *)expr4[-1] ;
         #endif
      }

   expr4[-2] = (char *) floatPtr ;
   expr4-- ;
}



void e4addDate()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *(double *)expr4-- = 0.0 ;
         return ;
      }
      else
   #endif
      {
         if ( v4functions[expr4infoPtr->functionI].type[0] == r4dateDoub )
         {
            if ( *(double *)expr4[-2] == 0.0 )
            {
               *(double *)expr4-- = 0.0 ;
               return ;
            }
         }
         else
         {
            if ( *(double *)expr4[-1] == 0.0 )
            {
               *(double *)expr4-- = 0.0 ;
               return ;
            }
         }
      }

   e4add() ;
}



void e4and()
{
   int i ;

   expr4 -= expr4infoPtr->numParms ;
   for( i = expr4infoPtr->numParms-1 ; i > 0 ; i-- )
      *(int *) expr4[0] = * (int *) expr4[i]  &&  * (int *) expr4[0] ;
   expr4++ ;
}



void e4concatSpecial( char movePchar )
{
   /* The total length of the result is in 'expr4infoPtrLen(expr4infoPtr)'. */

   int firstLen = expr4infoPtrLen( &(expr4infoPtr[-expr4infoPtr[-1].numEntries-1]) ) ;
   char *ptr = expr4[-2] ;
   int pos ;   // LY Jan 5/04 : moved declaration out of for-loop (Borland compiler error)

   for ( pos = firstLen-1; pos >= 0; pos-- )
   {
      if ( ptr[pos] != movePchar )
         break ;
      firstLen-- ;
   }

   // AS 10/04/00 - was problem here - evaluating 'constant' + calc() did not
   // copy the calc over because it is currently in the wrong spot.  Need to
   // call the c4memmove, even if pos == firstlen afterwards.  just don't need
   // to do the 2nd part...  - test t4calc.c
   // if ( ++pos < firstLen )
   // {
   //    lenTwo = expr4infoPtrLen(expr4infoPtr) - firstLen ;
   //    c4memmove( ptr+ pos, expr4[-1], (unsigned int)lenTwo ) ;
   //
   //    numChars = firstLen - pos ;
   //    memset( ptr+expr4infoPtrLen(expr4infoPtr)-numChars, movePchar, (unsigned int)numChars ) ;
   // }
   pos++ ;
   // AS Dec 17/04 - the length may actually be variable, so actually retrieve the length of the other expression...
   // lenTwo = expr4infoPtrLen(expr4infoPtr) - firstLen ;
   int maxLenTwo = expr4infoPtr->len - firstLen ;
   int lenTwo = min( maxLenTwo, expr4infoPtrLen(expr4infoPtr-1) ) ;

   if ( ( lenTwo > 0 ) && ( ( ptr + pos ) != expr4[-1] ) )  // dont copy if the positions are the same
      c4memmove( ptr + pos, expr4[-1], (unsigned int)lenTwo ) ;

   if ( pos <= firstLen )
   {
      // int numChars = firstLen - pos ;
      int numChars = firstLen + lenTwo ;
      c4memset( ptr+numChars, movePchar, (unsigned int)(expr4infoPtr->len - numChars) ) ;
   }

   if ( expr4infoPtr[-expr4infoPtr[-1].numEntries-1].varLength == 1 || expr4infoPtr[-1].varLength == 1 )
   {
      // means we are also variable length...this means that our length is also variable, so set the i2 paramater
      expr4infoPtr->varLength = 1 ;
      expr4infoPtr->i2 = firstLen + lenTwo ;
   }

   expr4-- ;
}



void e4concatTrim()
{
   e4concatSpecial(0) ;
}



void e4wstrLenCon()
{
   /* need to consider that the result won't be a true wstrLen since we can't concatenate the
      2 properly to achieve that.

      instead, what we produce is:

      wstrLen = <widestr><len>

      concat:

      <widestr1><len1> + <widestr2><len2> becomes: <widestr1><widestr2> --> for index drop the length
   */
   char *ptr = expr4[-2] ;
   int len = expr4infoPtr[-2].len ;

   c4memmove( ptr + len, expr4[-1], (unsigned int)expr4infoPtr[-1].len ) ;
   expr4-- ;
}



void e4concatTwo()
{
   e4concatSpecial(' ') ;
}



void e4contain()
{
   int aLen, compLen, i, logResult ;
   char firstChar, *bPtr ;

   logResult = 0 ;
   // LY Feb 7/05 : if value trimmed, use trimmed length instead of full length
   if ( expr4infoPtr[-expr4infoPtr[-1].numEntries-1].varLength )
      aLen = expr4infoPtr[-expr4infoPtr[-1].numEntries-1].i2 ;
   else
      aLen = expr4infoPtr[-expr4infoPtr[-1].numEntries-1].len ;
   firstChar = *expr4[-2] ;
   compLen = expr4infoPtr[-1].len - aLen ;
   bPtr = expr4[-1] ;

   /* See if there is a match */
   for ( i=0; i <= compLen; i++ )
   {
      if ( firstChar == bPtr[i] )
         if ( u4memcmp( expr4[-2], bPtr+i, (size_t)aLen ) == 0 )
         {
            logResult = 1 ;
            break ;
         }
   }

   expr4[-2] = expr4buf + expr4infoPtr->resultPos ;
   *(int *) expr4[-2] = logResult ;
   expr4-- ;
}



void e4copyConstant()
{
   void *ptr = *expr4++ = expr4buf+expr4infoPtr->resultPos ;
   #ifdef E4ANALYZE
      if ( ptr == 0 )
         error4( 0, e4info, E80903 ) ;
   #endif

   c4memcpy( ptr, expr4constants+ expr4infoPtr->i1, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
}



void e4copyToFloat()
{
   void *ptr = *expr4++ = expr4buf+expr4infoPtr->resultPos ;
   #ifdef E4ANALYZE
      if ( ptr == 0 )
         error4( 0, e4info, E80903 ) ;
   #endif

   assert5(expr4infoPtrLen(expr4infoPtr) == 8) ;
   double *dbl = (double *)(expr4constants + expr4infoPtr->i1) ;
   *((float *)ptr) = (float)(*dbl) ;
   // c4memcpy( ptr, dbl, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
}



void e4fieldCopy()
{
   void *ptr = *expr4++ = expr4buf+expr4infoPtr->resultPos ;
   #ifdef E4ANALYZE
      if ( ptr == 0 )
         error4( 0, e4info, E80903 ) ;
   #endif

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         c4memcpy( ptr, *(char **)expr4infoPtr->p1 + expr4infoPtr->i1, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



void e4copyParm()
{
   /* DTOS */
   void *ptr = expr4[-1] ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   #ifdef E4ANALYZE
      if ( ptr == 0 )
         error4( 0, e4info, E80903 ) ;
   #endif

   /* AS 11/20/97 FoxPro verified and MDX verified, DTOS( blank date )
      should produce <blank> not <0> which was previously being produced */
   if ( (unsigned int)expr4infoPtrLen(expr4infoPtr) == 8 )
      if ( c4memcmp( ptr, "       0", 8 ) == 0 )  /* change to blank */
      {
         c4memset( expr4[-1], ' ', 8 ) ;
         return ;
      }

   c4memcpy( expr4[-1], ptr, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
}



void e4ctod()
{
   char buf[8] ;
   double d ;

   date4init( buf, expr4[-1], expr4constants+ expr4infoPtr->i1 ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   d = date4long( buf ) ;
   c4memcpy( expr4[-1], (void *)&d, sizeof(d) ) ;
}



void e4date()
{
   char datePbuf[8] ;
   date4today( datePbuf ) ;
   *expr4++ = expr4buf + expr4infoPtr->resultPos ;
   *((double *) expr4[-1]) = (double) date4long( datePbuf ) ;
}



void e4day()
{
   double d ;
   d = (double) date4day( expr4[-1] ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   *(double *) expr4[-1] = d ;
}



void e4dayDoub()
{
   char datePbuf[8] ;
   date4assign( datePbuf, (long) *(double *)expr4[-1] ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   *(double *) expr4[-1] = (double) date4day( datePbuf ) ;
}



void e4del()
{
   expr4[0] = expr4buf + expr4infoPtr->resultPos ;
   expr4[0][0] = *( *(char **)expr4infoPtr->p1) ;
   expr4++ ;
}



void e4deleted()
{
   int result = 0 ;

   #ifdef E4MISC
      if ( *( *(char **)expr4infoPtr->p1 ) != '*' && *( *(char **)expr4infoPtr->p1 ) != ' ' )
         error4( 0, e4info, E80907 ) ;
   #endif

   if ( *( *(char **)expr4infoPtr->p1 ) == '*' )
      result = 1 ;

   *(int *) (*expr4++ = expr4buf + expr4infoPtr->resultPos ) = result ;
}



/*
void e4notDeleted()
{
   int result = 1 ;

   #ifdef E4MISC
      if ( *( *(char **)expr4infoPtr->p1 ) != '*' && *( *(char **)expr4infoPtr->p1 ) != ' ' )
         error4( 0, e4info, E80907 ) ;
   #endif

   if ( *( *(char **)expr4infoPtr->p1 ) == '*' )
      result = 0 ;

   *(int *) (*expr4++ = expr4buf + expr4infoPtr->resultPos ) = result ;
}
*/



void e4divide()
{
   double *resultPtr = (double *) (expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *resultPtr = 0 ;
      }
      else
   #endif
      {
         double doub ;
         c4memcpy( (void *)&doub, (void *)expr4[-1], sizeof(double ) ) ;
         if ( doub == 0.0 )
            *resultPtr = 0.0 ;
         else
         {
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               {
                  double a, b ;
                  memcpy( &a, expr4[-2], sizeof(double) ) ;
                  memcpy( &b, expr4[-1], sizeof(double) ) ;
                  *resultPtr = a / b ;
               }
            #else
               *resultPtr = *(double *)expr4[-2] / *(double *) expr4[-1] ;
            #endif
         }
      }

   expr4[-2] = (char *) resultPtr ;
   expr4-- ;
}



void e4divideFlt()
{
   float *resultPtr = (float *) (expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *resultPtr = 0 ;
      }
      else
   #endif
      {
         float flt ;
         c4memcpy( (void *)&flt, (void *)expr4[-1], sizeof(float ) ) ;
         if ( flt == 0.0 )
            *resultPtr = 0.0 ;
         else
         {
            #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE */
               assert5( 0 ) ; // not supported
               // {
               //    float a, b ;
               //    memcpy( &a, expr4[-2], sizeof(float) ) ;
               //    memcpy( &b, expr4[-1], sizeof(float) ) ;
               //    *resultPtr = a / b ;
               // }
            #else
               *resultPtr = *(float *)expr4[-2] / *(float *) expr4[-1] ;
            #endif
         }
      }

   expr4[-2] = (char *) resultPtr ;
   expr4-- ;
}



// AS Apr 22/04 - support for TTOC
void e4ttoc()
{
   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   c4memcpy( ptr, expr4[-1], 8 ) ;
   memcpy( ptr+8, "000000", 6 ) ;
   expr4[-1] = ptr ;
}



void e4ttocDateTime()
{
/*
   int numParms = 3 ;
   long time = expr4infoPtr->i1 ;
   double year = 0 ;
   double month = 0 ;
   double day = 0 ;

   year = *(double *)expr4[-numParms] ;
   month = *(double *)expr4[1-numParms] ;
   day = *(double *)expr4[2-numParms] ;

   char *buf = expr4constants+ expr4infoPtr->i1 ;
*/

   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   /* LY Jun 22/04 : change assignment methods to date & time vars for S4DATA_ALIGN */
   S4LONG date, time ;
   #ifdef S4DATA_ALIGN
      memcpy( &date, expr4[-1], sizeof(S4LONG) ) ;
      memcpy( &time, expr4[-1] + sizeof(S4LONG), sizeof(S4LONG) ) ;
   #else
      date = ((long *)expr4[-1])[0] ;
      time = ((long *)expr4[-1])[1] ;
   #endif
   date4assignLow( ptr, date, 0 ) ;
   // the time part is in milliseconds...
   S4LONG seconds, minutes, hours, val ;

   ptr += 8 ;
   val = time / 1000 ;  // remove the thousandths's of seconds

   seconds = val % 60 ;
   val /= 60 ;
   minutes = val % 60 ;
   val /= 60 ;
   hours = val ;

//   c4ltoa45( year, ptr, -4 ) ;  // - means fill with 0s
//   c4ltoa45( month, ptr+2, -2 ) ;  // - means fill with 0s
//   c4ltoa45( day, ptr+4, -2 ) ;  // - means fill with 0s
   c4ltoa45( hours, ptr, -2 ) ;  // - means fill with 0s
   c4ltoa45( minutes, ptr+2, -2 ) ;  // - means fill with 0s
   c4ltoa45( seconds, ptr+4, -2 ) ;  // - means fill with 0s
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
}



void e4ttocDoub()
{
   e4dtosDoub() ;
   e4ttoc() ;
}



void e4dtoc()
{
   char buf[LEN4DATE_FORMAT] ;

   date4format( expr4[-1], buf, expr4constants+ expr4infoPtr->i1 ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   c4memcpy( expr4[-1], buf, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
}



void e4dateTime( void )
{
   // this is just a conversion of a static value...
   int numParms = 3 ;
   long timePart = expr4infoPtr->i1 ;
   double year = 0 ;
   double month = 0 ;
   double day = 0 ;

   year = *(double *)expr4[-numParms] ;
   month = *(double *)expr4[1-numParms] ;
   day = *(double *)expr4[2-numParms] ;

   long datePart = date4yymmddToJulianLong( (int)year, (int)month, (int)day ) ;

   long *result = (long *)expr4[-numParms] ;
   result[0] = datePart ;
   result[1] = timePart ;
   expr4 -= (numParms-1) ;
}



void e4dtocDoub()
{
   e4dtosDoub() ;
   e4dtoc() ;
}



// AS July 25/02 - Added new function SPACE(N)
void e4space()
{
   // just creates 'n' spaces...
   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   c4memset( ptr, ' ', expr4infoPtrLen(expr4infoPtr) ) ;
}



void e4chr()
{
   double doub ;

   c4memcpy( (void *) &doub, (void *) expr4[-1], sizeof(double) ) ;
   *expr4[-1] = (char) doub ;
}



void e4dtosDoub()
{
   date4assign( expr4buf + expr4infoPtr->resultPos, (long) *(double *) expr4[-1] ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
}



void e4equalCur()
{
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
         *intPtr = !currency4compare( (CURRENCY4 *)expr4[-2], (CURRENCY4 *)expr4[-1] )  ;

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



void e4equalDtTime()
{
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;
   #ifdef S4CLIENT_OR_FOX
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
         *intPtr = !date4timeCompare( (S4LONG *)expr4[-2], (S4LONG *)expr4[-1] )  ;

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



void e4equal()
{
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
         *intPtr = !u4memcmp( expr4[-2], expr4[-1], (unsigned int)expr4infoPtr->i1 )  ;

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



void e4false()
{
   int *ptr ;

   ptr = (int *) (*expr4++ = expr4buf+expr4infoPtr->resultPos) ;
   *ptr = 0 ;
}



void e4fieldDateD()
{
   void *ptr = *expr4++ = expr4buf+expr4infoPtr->resultPos ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( double ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         double d = date4long( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ;
         #ifdef E4ANALYZE
            if ( ptr == 0 )
               error4( 0, e4info, E80903 ) ;
         #endif
         c4memcpy( ptr, (void *)&d, sizeof(d) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



void e4fieldLog()
{
   int *ptr = (int *) (*expr4++ = expr4buf+expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, 1 ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         char charValue = *(* (char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ;
         if ( charValue == 'Y'  ||  charValue == 'y'  ||
               charValue == 'T'  ||  charValue == 't'  )
            *ptr = 1 ;
         else
            *ptr = 0 ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



#ifndef S4OFF_MEMO
   void e4fieldMemo()
   {
      char *ptr, *memoPtr ;
      unsigned memoLen, copyLen, zeroLen ;

      ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;

      #ifdef S4CLIENT_OR_FOX
         // AS Feb 20/04 - support for r4null handling
         assert5( expr4infoPtr->fieldPtr != 0 ) ;
         if ( f4null( expr4infoPtr->fieldPtr ) )
         {
            memset( ptr, 0, expr4infoPtrLen(expr4infoPtr) ) ;
            expr4infoPtr->isNull = 1 ;
         }
         else
         {
            expr4infoPtr->isNull = 0 ;
      #endif
            /*   memoLen = f4memoLen( d4fieldJ( expr4ptr->data, expr4infoPtr->fieldNo ) ) ;*/
            /*   memoPtr = f4memoPtr( d4fieldJ( expr4ptr->data, expr4infoPtr->fieldNo ) ) ;*/
            memoLen = f4memoLen( expr4infoPtr->fieldPtr ) ;
            memoPtr = f4memoPtr( expr4infoPtr->fieldPtr ) ;
            if( error4code( expr4ptr->codeBase ) < 0 )
               return ;

            copyLen = memoLen ;
            zeroLen = 0 ;
            if( copyLen > (unsigned) expr4infoPtrLen(expr4infoPtr) )
               copyLen = (unsigned int)expr4infoPtrLen(expr4infoPtr) ;
            else
               zeroLen = expr4infoPtrLen(expr4infoPtr) - copyLen ;

            #ifdef E4ANALYZE
               if ( ptr == 0 )
                  error4( 0, e4info, E80903 ) ;
            #endif
            memcpy( ptr, memoPtr, copyLen ) ;
            memset( ptr + copyLen, 0, zeroLen ) ;
      #ifdef S4CLIENT_OR_FOX
         }
      #endif
   }
#endif



void e4fieldCurD()
{
   void *ptr ;
   double d ;
   CURRENCY4 *val ;
   char currencyBuffer[21] ;

   ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( double ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         val = ((CURRENCY4 *)( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ) ;

         /*
            this algorithm produces values which might be off at the last point
            of double precision (comparet to string calculations), so instead
            the slower string conversion routines were inserted */
         /*
            d = val->lo[3] ;
            d = d * 65536 + (double)val->lo[2] ;
            d = d * 65536 + (double)val->lo[1] ;
            d = d * 65536 + (double)val->lo[0] ;
            d /= 10000.0 ;
         */

         c4memset( currencyBuffer, 0, sizeof( currencyBuffer ) ) ;
         c4currencyToA( currencyBuffer, sizeof( currencyBuffer ), val, 4, 0 ) ;
         d = c4atod( currencyBuffer, c4strlen( currencyBuffer ) ) ;

         #ifdef E4ANALYZE
            if ( ptr == 0 )
               error4( 0, e4info, E80903 ) ;
         #endif
         c4memcpy( ptr, (void *)&d, sizeof(d) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



// AS Jul 21/05 - Support for new field type binary float
void e4fieldIntF()
{
   void *ptr ;
   float flt ;
   S4LONG val ;   /* LY July 16/03 : changed from long for 64-bit */

   ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( float ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         #ifdef S4DATA_ALIGN
            memcpy( &val, *(char **)expr4infoPtr->p1 + expr4infoPtr->i1, sizeof(S4LONG) ) ;
         #else
            val = *((S4LONG *)( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ) ;
         #endif
         #ifdef S4BYTE_SWAP
            assert5( 0 ) ;  // not supported
            // flt = (float)x4reverseLong( &val ) ;
         #else
            flt = (float)val ;
         #endif

         #ifdef E4ANALYZE
            if ( ptr == 0 )
               error4( 0, e4info, E80903 ) ;
         #endif
         c4memcpy( ptr, (void *)&flt, sizeof(flt) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}


/*
void e4fieldFloatD()
{
   void *ptr ;
   double d ;
   float val ;

   ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( double ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         #ifdef S4DATA_ALIGN
            memcpy( &val, *(char **)expr4infoPtr->p1 + expr4infoPtr->i1, sizeof(float) ) ;
         #else
            val = *((float *)( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ) ;
         #endif
         #ifdef S4BYTE_SWAP
            d = (double)x4reverseFloat( &val ) ;
         #else
            d = (double)val ;
         #endif

         #ifdef E4ANALYZE
            if ( ptr == 0 )
               error4( 0, e4info, E80903 ) ;
         #endif
         c4memcpy( ptr, (void *)&d, sizeof(d) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}
*/


void e4fieldIntD()
{
   void *ptr ;
   double d ;
   S4LONG val ;   /* LY July 16/03 : changed from long for 64-bit */

   ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( double ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE*/
            /* LY July 7/16 : changed from sizef(long) for 64-bit */
            memcpy( &val, *(char **)expr4infoPtr->p1 + expr4infoPtr->i1, sizeof(S4LONG) ) ;
         #else
            /* LY July 7/16 : changed from (long *) for 64-bit */
            val = *((S4LONG *)( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ) ;
         #endif
         #ifdef S4BYTE_SWAP
            d = (double)x4reverseLong( &val ) ;
         #else
            d = (double)val ;
         #endif

         #ifdef E4ANALYZE
            if ( ptr == 0 )
               error4( 0, e4info, E80903 ) ;
         #endif
         c4memcpy( ptr, (void *)&d, sizeof(d) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



void e4fieldNumD()
{
   void *ptr ;
   double d ;

   ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( double ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         d = c4atod( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1, expr4infoPtrLen(expr4infoPtr) ) ;
         #ifdef E4ANALYZE
            if ( ptr == 0 )
               error4( 0, e4info, E80903 ) ;
         #endif
         c4memcpy( ptr, (void *)&d, sizeof(d) ) ;
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



// AS July 27/01 - field value is stored in intel ordering always, so need a special function to extract
// the value in local double ordering format
void e4fieldDoubD()
{
   /* used to take the double format from the data file (in intel format), and convert it to
      a local double type
   */
   void *ptr = *expr4++ = expr4buf + expr4infoPtr->resultPos ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
      {
         memset( ptr, 0, sizeof( double ) ) ;
         expr4infoPtr->isNull = 1 ;
      }
      else
      {
         expr4infoPtr->isNull = 0 ;
   #endif
         #ifdef S4BYTE_SWAP  /* LY 2001/07/28 */
            double tempDoub, d ;
            memcpy( &tempDoub, *(char **)expr4infoPtr->p1 + expr4infoPtr->i1, sizeof(double) ) ;
            d = x4reverseDouble( &tempDoub ) ;
            #ifdef E4ANALYZE
               if ( ptr == 0 )
                  error4( 0, e4info, E80903 ) ;
            #endif
            c4memcpy( ptr, &d, sizeof( double ) ) ;
         #else
            // for intel, this is just doing a simple copy from the field buffer...
            double *d = (double *)( *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ) ;
            #ifdef E4ANALYZE
               if ( ptr == 0 )
                  error4( 0, e4info, E80903 ) ;
            #endif
            c4memcpy( ptr, d, sizeof( double ) );
         #endif
   #ifdef S4CLIENT_OR_FOX
      }
   #endif
}



void e4greaterCur()
{
   int *intPtr;
   int rc ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         rc = currency4compare( (CURRENCY4 *)expr4[-2], (CURRENCY4 *)expr4[-1] )  ;

         if( rc > 0 )
            *intPtr = 1 ;
         else
         {
            if( rc < 0 )
               *intPtr = 0 ;
            else
               *intPtr = (int)((long)expr4infoPtr->p1) ;
         }
      }

   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4greaterDtTime()
{
   int *intPtr;
   int rc ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         /* LY 2001/07/28 : changed from long* to S4LONG* for 64-bit */
         rc = date4timeCompare( (S4LONG *)expr4[-2], (S4LONG *)expr4[-1] )  ;

         if( rc > 0 )
            *intPtr = 1 ;
         else
         {
            if( rc < 0 )
               *intPtr = 0 ;
            else
               *intPtr = (int)((long)expr4infoPtr->p1) ;
         }
      }
   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4greater()
{
   int *intPtr;
   int rc ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         rc = u4memcmp( expr4[-2], expr4[-1], (unsigned int)expr4infoPtr->i1 ) ;

         if( rc > 0 )
            *intPtr = 1 ;
         else
         {
            if( rc < 0 )
               *intPtr = 0 ;
            else
               *intPtr = (int)((long)expr4infoPtr->p1) ;
         }
      }
   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4greaterDoub()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         *(int *)expr4[-1] = 0 ;
      }
      else
   #endif
      {
         e4lessEqDoub() ;
         *(int *)expr4[-1] = ! *(int *)expr4[-1] ;
      }
}



void e4greaterFloat()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         *(int *)expr4[-1] = 0 ;
      }
      else
   #endif
      {
         e4lessEqFloat() ;
         *(int *)expr4[-1] = ! *(int *)expr4[-1] ;
      }
}



void e4greaterEqCur()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         *(int *)expr4[-1] = 0 ;
      }
      else
   #endif
      {
         e4lessCur() ;
         *((int *)expr4[-1]) = ! *((int *)expr4[-1]) ;
      }
}



void e4greaterEqDtTime()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         #ifdef S4DATA_ALIGN  // LY Jun 22/04
            memset( expr4[-1], 0, sizeof(int) ) ;
         #else
            *(int *)expr4[-1] = 0 ;
         #endif
      }
      else
   #endif
      {
         e4lessDtTime() ;
         *((int *)expr4[-1]) = ! *((int *)expr4[-1]) ;
      }
}



void e4greaterEq()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         #ifdef S4DATA_ALIGN  // LY Jun 22/04
            memset( expr4[-1], 0, sizeof(int) ) ;
         #else
            *(int *)expr4[-1] = 0 ;
         #endif
      }
      else
   #endif
      {
         e4less() ;
         *((int *)expr4[-1]) = ! *((int *)expr4[-1]) ;
      }
}



void e4greaterEqDoub()
{
   int *intPtr ;
   #ifdef S4DATA_ALIG2
      double a,b;
   #endif

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            memcpy(&a, (double *)expr4[-2], sizeof(double));
            memcpy(&b, (double *)expr4[-1], sizeof(double));
            if (a>=b)
         #else
            if ( *(double *)expr4[-2] >= *(double *)expr4[-1] )
         #endif /* !S4DATA_ALIG2 */
            *intPtr = 1 ;
         else
            *intPtr = 0 ;
      }
   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4greaterEqFloat()
{
   int *intPtr ;
   #ifdef S4DATA_ALIG2
      float a,b;
   #endif

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            assert5( 0 ) ;  // not supported
            // memcpy(&a, (float *)expr4[-2], sizeof(float));
            // emcpy(&b, (float *)expr4[-1], sizeof(float));
            if (a>=b)
         #else
            if ( *(float *)expr4[-2] >= *(float *)expr4[-1] )
         #endif /* !S4DATA_ALIG2 */
            *intPtr = 1 ;
         else
            *intPtr = 0 ;
      }
   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4iif()
{
   if ( *(int *) expr4[-3] )
      c4memmove( expr4buf + expr4infoPtr->resultPos, expr4[-2], (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
   else
      c4memmove( expr4buf + expr4infoPtr->resultPos, expr4[-1], (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
   expr4[-3] = expr4buf + expr4infoPtr->resultPos ;
   expr4-= 2 ;
}



void e4iifStr()
{
   /* AS 11/16/99 -- special e4iif for string results only to allow for varying lengths... */
   int lenCopy, numBlanks ;
   if ( *(int *) expr4[-3] )
   {
      // AS 11/15/99 -- to allow varying lengths, use length from expr4infoPtr, and blank out remainder...
      lenCopy = (unsigned int)expr4infoPtr->i1 ;
      c4memmove( expr4buf + expr4infoPtr->resultPos, expr4[-2], lenCopy ) ;
      numBlanks = (unsigned int)expr4infoPtrLen(expr4infoPtr) - lenCopy ;
      c4memset( expr4buf + expr4infoPtr->resultPos + lenCopy, ' ', numBlanks ) ;
   }
   else
   {
      lenCopy = (unsigned int)expr4infoPtr->i2 ;
      c4memmove( expr4buf + expr4infoPtr->resultPos, expr4[-1], lenCopy ) ;
      numBlanks = (unsigned int)expr4infoPtrLen(expr4infoPtr) - lenCopy ;
      c4memset( expr4buf + expr4infoPtr->resultPos + lenCopy, ' ', numBlanks ) ;
   }
   // AS 11/15/99 --> try to selectively move the buffer pointer over... - may be extra chars at end to remove...
   expr4[-3] = expr4buf + expr4infoPtr->resultPos ;
   expr4-= 2 ;
}



void e4lessCur()
{
   int *intPtr;
   int rc ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         rc = currency4compare( (CURRENCY4 *)expr4[-2], (CURRENCY4 *)expr4[-1] )  ;

         if( rc < 0 )
            *intPtr = 1 ;
         else
         {
            if( rc > 0 )
               *intPtr = 0 ;
            else
               *intPtr = (int)((long)expr4infoPtr->p1) ;
         }
      }

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



void e4lessDtTime()
{
   int *intPtr;
   int rc ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         /* LY 2001/07/28 : changed from long* to S4LONG* for 64-bit */
         rc = date4timeCompare( (S4LONG *)expr4[-2], (S4LONG *)expr4[-1] )  ;

         if( rc < 0 )
            *intPtr = 1 ;
         else
         {
            if( rc > 0 )
               *intPtr = 0 ;
            else
               *intPtr = (int)((long)expr4infoPtr->p1) ;
         }
      }

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



// AS Jan 6/05 - Added support for a new KEYEQUAL function
void e4keyEqual()
{
   // this function converts the 2 input str entries into keys (assuming a tag) and then compares them.
   // if there is no tag, a simple string comparison is done.
   // if the length of the 2 strings does not match, a partial compare is done.
   // this is useful for example to create a filter on a partial seek that works with general collation sequences (discounts tails
   // for partial seeks), which otherwise would only match the complete matching character
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
      {
         // AS Sep 7/05 - not supported in client version...
         #ifdef S4CLIENT
            error4( expr4ptr->codeBase, e4notSupported, E90906 ) ;
            *intPtr = 0 ;
         #else
            #ifndef S4OFF_INDEX
               TAG4FILE *tagPtr = expr4infoPtr->tagPtr ;
               if ( tagPtr == 0 )    // no tag so just a memcmp
            #endif
                  *intPtr = !u4memcmp( expr4[-2], expr4[-1], (unsigned int)expr4infoPtr->i1 )  ;
            #ifndef S4OFF_INDEX
               else
               {
                  // must create the keys and do the compare
                  char buf1[I4MAX_KEY_SIZE], buf2[I4MAX_KEY_SIZE] ;
                  unsigned int len = expr4infoPtr->i1 ;
                  tfile4stok( tagPtr, buf1, expr4[-2], len ) ;
                  tfile4stok( tagPtr, buf2, expr4[-1], len ) ;
                  if ( len < (unsigned int)tfile4keyLen( tagPtr ) )
                     *intPtr = !u4keycmpPartial( buf1, buf2, len, len, 0, collation4get( tagPtr->collateName ), 0, 0 ) ;
                  else
                     *intPtr = !u4keycmp( buf1, buf2, len, len, 0, collation4get( tagPtr->collateName ) ) ;
               }
            #endif
         #endif
      }
   #else
      *intPtr = !u4memcmp( expr4[-2], expr4[-1], (unsigned int)expr4infoPtr->i1 )  ;
   #endif

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



void e4less()
{
   int *intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;
   int rc ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         rc = u4memcmp( expr4[-2], expr4[-1], (unsigned int)expr4infoPtr->i1 ) ;

         if( rc < 0 )
            *intPtr = 1 ;
         else
         {
            if( rc > 0 )
               *intPtr = 0 ;
            else
               *intPtr = (int)((long)expr4infoPtr->p1) ;
         }
      }

   expr4[-2] = (char *)intPtr ;
   expr4-- ;
}



void e4lessDoub()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         *(int *)expr4[-1] = 0 ;
      }
      else
   #endif
      {
         e4greaterEqDoub() ;
         *(int *)expr4[-1] = ! *(int *)expr4[-1] ;
      }
}



void e4lessFloat()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         *(int *)expr4[-1] = 0 ;
      }
      else
   #endif
      {
         e4greaterEqFloat() ;
         *(int *)expr4[-1] = ! *(int *)expr4[-1] ;
      }
}



void e4lessEqCur()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         *(int *)expr4[-1] = 0 ;
      }
      else
   #endif
      {
         e4greaterCur() ;
         *((int *)expr4[-1]) = ! *((int *)expr4[-1]) ;
      }
}



void e4lessEqDtTime()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         #ifdef S4DATA_ALIGN  // LY Jun 22/04
            memset( expr4[-1], 0, sizeof(int) ) ;
         #else
            *(int *)expr4[-1] = 0 ;
         #endif
      }
      else
   #endif
      {
         e4greaterDtTime() ;
         *((int *)expr4[-1]) = ! *((int *)expr4[-1]) ;
      }
}



void e4lessEq()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         expr4-- ;
         #ifdef S4DATA_ALIGN  // LY Jun 22/04
            memset( expr4[-1], 0, sizeof(int) ) ;
         #else
            *(int *)expr4[-1] = 0 ;
         #endif
      }
      else
   #endif
      {
         e4greater() ;
         *((int *)expr4[-1]) = ! *((int *)expr4[-1]) ;
      }
}



void e4lessEqDoub()
{
   int *intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            double doub1, doub2 ;

            memcpy( (void *)&doub1, (void *)expr4[-2], sizeof(double) ) ;
            memcpy( (void *)&doub2, (void *)expr4[-1], sizeof(double) ) ;
         #endif
         #ifdef S4DATA_ALIG2
            if ( doub1 <= doub2 )
         #else
            if ( *(double *)expr4[-2] <= *(double *)expr4[-1] )
         #endif
            *intPtr = 1 ;
         else
            *intPtr = 0 ;
      }
   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4lessEqFloat()
{
   int *intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            float flt1, flt2 ;

            memcpy( (void *)&flt1, (void *)expr4[-2], sizeof(float) ) ;
            memcpy( (void *)&flt2, (void *)expr4[-1], sizeof(float) ) ;
         #endif
         #ifdef S4DATA_ALIG2
            if ( flt1 <= flt2 )
         #else
            if ( *(float *)expr4[-2] <= *(float *)expr4[-1] )
         #endif
            *intPtr = 1 ;
         else
            *intPtr = 0 ;
      }
   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4ltrim()
{
   int n ;
   char *ptr ;

   for( n = 0; n < expr4infoPtr->len ; n++ )
   {
      if ( expr4[-1][n] != ' ' && expr4[-1][n] != 0 )
         break ;
   }

   // AS Dec 16/04 - use i2 to store resulting trimmed length
   expr4infoPtr->i2 = expr4infoPtr->len - n ;
   expr4infoPtr->varLength = 1 ;

   ptr = expr4buf + expr4infoPtr->resultPos ;
   c4memmove( ptr, expr4[-1]+n, (unsigned int)(expr4infoPtr->len - n) ) ;
   c4memset( ptr + expr4infoPtr->len - n, 0, (unsigned int)n ) ;
   expr4[-1] = ptr ;
}



void e4alltrim()
{
   // AS Dec 16/04 - i2 is used to store resulting trimmed length, we need to track this for both parts of the trim...
   e4ltrim() ;
   int i2ltrim = expr4infoPtr->i2 ;  // the length after the first trim
   e4trim() ;
   int i2trim = expr4infoPtr->i2 ;  // the length after the second trim
   // the final trim length will be the i2ltrim less the difference on the i2trim
   // expr4infoPtr->i2 = i2ltrim - (expr4infoPtr->len-i2trim) ;
   expr4infoPtr->i2 = i2trim ;
   expr4infoPtr->varLength = 1 ;
}



void e4month()
{
   double *doublePptr = (double *)(expr4buf + expr4infoPtr->resultPos) ;
   *doublePptr = (double)date4month( expr4[-1] ) ;
   expr4[-1] = (char *) doublePptr ;
}



void e4monthDoub()
{
   char datePbuf[8] ;
   double *doublePptr ;

   doublePptr = (double *) (expr4buf + expr4infoPtr->resultPos) ;
   date4assign( datePbuf, (long) *(double *)expr4[-1] ) ;
   *doublePptr = (double) date4month( datePbuf ) ;
   expr4[-1] = (char *) doublePptr ;
}



void e4multiply()
{
   double *doublePtr ;

   doublePtr = (double *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *doublePtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE*/
            double a, b ;
            memcpy( &a, expr4[-2], sizeof(double) ) ;
            memcpy( &b, expr4[-1], sizeof(double) ) ;
            *doublePtr = a * b ;
         #else
            *doublePtr = *(double *)expr4[-2] * *(double *)expr4[-1] ;
         #endif
      }

   expr4[-2] = (char *)doublePtr ;
   expr4-- ;
}



void e4multiplyFlt()
{
   float *floatPtr ;

   floatPtr = (float *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *floatPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIGN  /* LY 00/07/24 : for S4WINCE*/
            assert5( 0 ) ;  // not supported
            // float a, b ;
            // memcpy( &a, expr4[-2], sizeof(float) ) ;
            // memcpy( &b, expr4[-1], sizeof(float) ) ;
            // *floatPtr = a * b ;
         #else
            *floatPtr = *(float *)expr4[-2] * *(float *)expr4[-1] ;
         #endif
      }

   expr4[-2] = (char *)floatPtr ;
   expr4-- ;
}



void e4nop()
{
}



void e4not()
{
   int *ptr = (int *)expr4[-1] ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      if ( (expr4infoPtr-1)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *ptr = 0 ;
      }
      else
   #endif
         *ptr = !*ptr ;
}



void e4notEqualCur()
{
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 1 ;
      }
      else
   #endif
         *intPtr = currency4compare( (CURRENCY4 *)expr4[-2], (CURRENCY4 *)expr4[-1] )  ;

   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4notEqualDtTime()
{
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr + firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 1 ;
      }
      else
   #endif
         *intPtr = date4timeCompare( (S4LONG *)expr4[-2], (S4LONG *)expr4[-1] )  ;

   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4notEqual()
{
   int *intPtr ;

   intPtr = (int *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr + firstEntry)->isNull != (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *intPtr = 1 ;
      }
      else
   #endif
         *intPtr = u4memcmp( expr4[-2], expr4[-1], (unsigned int)expr4infoPtr->i1 ) != 0 ? 1 : 0  ;

   expr4[-2] = (char *) intPtr ;
   expr4-- ;
}



void e4or()
{
   int i ;

   expr4 -= expr4infoPtr->numParms ;
   for( i = expr4infoPtr->numParms-1 ; i > 0 ; i-- )
   {

      *(int *) expr4[0] = * (int *) expr4[i] || * (int *) expr4[0] ;
   }
   expr4++ ;
}



void e4fieldAdd()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      assert5( expr4infoPtr->fieldPtr != 0 ) ;
      if ( f4null( expr4infoPtr->fieldPtr ) )
         expr4infoPtr->isNull = 1 ;
      else
         expr4infoPtr->isNull = 0 ;
   #endif
   *expr4++ = *(char **)expr4infoPtr->p1 + expr4infoPtr->i1 ;
}



void e4parmRemove()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr+firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
      }
   #endif

   // AS Dec 15/04 - support for subexpressions...mark as variable in this case
   expr4infoPtr->varLength = 1 ;
   expr4infoPtr->i2 = expr4infoPtr->len ;
   expr4-- ;
}



void e4padLeft()
{
   // similar to pad right, except that we need to take into account that a trim may mean
   // that we want to pad more bytes on the left that is indicated by our infoPtr->i1 values.
   // therefore, calculate the shift length first...
   // then take the string, shift it to the right of the buffer, and then replace with blanks
   // the previous bytes...

   int shiftLength = expr4infoPtr->i1 ;  // for simple expressions, no trims...

   // now scan the end of the 'valid' entry and count the number of nulls which need to
   // be replaced with blanks on the left. -- basically this just reduces the amount we need
   // to shift, but increases the amount we need to blank.
   char *valuePtr = expr4buf + expr4infoPtr->resultPos ;  // points to the start of the buffer
   int maxCheckBytes = expr4infoPtrLen(expr4infoPtr) - expr4infoPtr->i1 ;
   for ( ; maxCheckBytes > 0 ; maxCheckBytes-- )
   {
      if ( valuePtr[maxCheckBytes-1] == 0 )
         shiftLength++ ;
      else  // must be no more nulls
         break ;
   }

   // need to move from the start of the buffer to start+shiftlength
   // AS 12/20/00 - User indicated problem with function...
   // c4memmove( valuePtr + shiftLength, valuePtr, shiftLength ) ;
   c4memmove( valuePtr + shiftLength, valuePtr, expr4infoPtr[-1].len ) ;
   c4memset( valuePtr, ' ', shiftLength ) ;
}



void e4padRight()
{
   // i1 contains the amount of bytes needed to memset to blank at the end of the expression
   // - i.e. our length minus this amount...
   c4memset( expr4buf + expr4infoPtr->resultPos + expr4infoPtrLen(expr4infoPtr) - expr4infoPtr->i1, ' ', expr4infoPtr->i1 ) ;

   // now take into account to remove any null bytes in the expression which may have been
   // inserted by a trim or other function which reduced the length of the expression without
   // an explicit length value (i.e. the info ptr for a trim has the resulting length including
   // the nulls or the trimmed spaces)

   char *valuePtr = expr4buf + expr4infoPtr->resultPos ;
   int maxCheckBytes = expr4infoPtrLen(expr4infoPtr) - expr4infoPtr->i1 ;
   for ( ; maxCheckBytes > 0 ; maxCheckBytes-- )
   {
      if ( valuePtr[maxCheckBytes-1] == 0 )
         valuePtr[maxCheckBytes-1] = ' ' ;
      else  // must be no more nulls
         break ;
   }
//   throw Err5internal() ; // not coded yet
}



#ifndef S4NO_POW
   void e4power()
   {
      double *doublePptr ;
      #ifdef S4DATA_ALIG2
         double doub1,doub2,doub3 ;
      #endif
      doublePptr = (double *) (expr4buf + expr4infoPtr->resultPos) ;
      #ifdef S4DATA_ALIG2
         memcpy( &doub2, expr4[-2], sizeof(double) ) ;
         memcpy( &doub1, expr4[-1], sizeof(double) ) ;
         doub3 = pow( doub2, doub1 ) ;
         memcpy( doublePptr, &doub3, sizeof(double) ) ;
      #else
         *doublePptr = pow( *(double *) expr4[-2], *(double *) expr4[-1] ) ;
      #endif
      expr4[-2] = (char *) doublePptr ;
      expr4-- ;
   }
#endif



void e4recCount()
{
   double d ;
   long serverId = -2L ;
   DATA4FILE *dataFile ;
   #ifdef S4SERVER
      DATA4 *data ;
      LIST4 *dataList ;
   #endif

   dataFile = (DATA4FILE *)expr4infoPtr->p1 ;

   #ifdef S4SERVER
      /* need to get a DATA4 corresponding to the data4file for the current
         client, in order to determine the context, which may affect the current
         record count for the data file due to transactions */
      dataList = tran4dataList( code4trans( dataFile->c4 ) ) ;
      for ( data = 0 ;; )
      {
         data = (DATA4 *)l4next( dataList, data ) ;
         if ( data == 0 )
         {
            serverId = -1 ;   /* get non-trans count */
            break ;
         }
         if ( data->dataFile == dataFile )
         {
            serverId = data4serverId( data ) ;
            break ;
         }
      }
   #endif

   d = (double)dfile4recCount( dataFile, serverId ) ;
   c4memcpy( *expr4++ = expr4buf+ expr4infoPtr->resultPos, (void *)&d, sizeof(d) ) ;
}



void e4recno()
{
   double d ;

   d = (double)d4recNo( expr4ptr->data ) ;  /* updated for c/s */
   /*   d = (double)d4recNo( (DATA4 *) expr4infoPtr->p1 ) ;*/
   c4memcpy( *expr4++ = expr4buf+ expr4infoPtr->resultPos, (void *)&d, sizeof(d) ) ;
}



void e4stod()
{
   double *doublePptr ;
   #ifdef S4DATA_ALIG2
      double a;
   #endif

   doublePptr = (double *)(expr4buf + expr4infoPtr->resultPos) ;
   #ifdef S4DATA_ALIG2
      a=(double)date4long(expr4[-1]);
      memcpy(doublePptr, &a, sizeof(double));
   #else
      *doublePptr = (double) date4long( expr4[-1] ) ;
   #endif  /* !S4DATA_ALIG2  */
   expr4[-1] = (char *) doublePptr ;
}



void e4strZero()
{
   /* AS 07/19/99 - added new function - StrZero( nVal, nLen ), nVal is a numeric value, nLen is
      a numeric len.  The result is right justified with zeros filling any spaces on the left.
      eg. StrZero( 100.03, 10 ) yields "0000100.03"

      StrZero( 100. 2 ) is an overflow and yeilds "**"
      StrZero( .0004, 3 ) truncates to "000"
      StrZero( 1.44, 2) becomes "01" because there is no room for any decimals and we truncate
   */

   /*
      AS Dec 12/03 - implementation incorrect.  3rd paramater should be the number of decimal places
      - Same as e4str() but replace blanks with zeros
   */

   char *ptr ;
   #ifdef S4DATA_ALIG2
        double doub;
   #endif

   ptr = expr4buf + expr4infoPtr->resultPos ;
   short len = expr4infoPtrLen(expr4infoPtr) ;
   #ifdef S4DATA_ALIG2
      memcpy( (void *)&doub, expr4[-1], sizeof(double) );
      c4dtoa45( doub, ptr, len, expr4infoPtr->i1 ) ;
   #else
      c4dtoa45( *(double *) expr4[-1], ptr, len, expr4infoPtr->i1 ) ;
   #endif

   for ( int lp = 0 ; lp < len ; lp++ )
   {
      if ( ptr[lp] == ' ' )
         ptr[lp] = '0' ;
      else
         break ;
   }

   expr4[-1] = ptr ;

/*
   char *ptr ;
   #ifdef S4DATA_ALIG2
        double doub;
   #endif
   char fillerChar = '0' ;
   short numDecimals = expr4infoPtr->i1 ;

   ptr = expr4buf + expr4infoPtr->resultPos ;
   // Basically, what we have is a double value that needs to be converted to string and
   // have any extra '0's after the decimal point be placed on the left of the string (i.e.
   // right justify the number and put 0's on the left)...

   // calculate length of number before decimals...
   short numSpotsBeforeDecimal = 0 ;  // leave room for the decimal if it exists...
   double doubleValue = *(double *) expr4[-1] ;
   if ( doubleValue < 0 )
   {
      doubleValue = -doubleValue ;
      numSpotsBeforeDecimal++ ;  // leave spot for '-' sign
   }
   while ( doubleValue >= 1 )
   {
      doubleValue /= 10 ;
      numSpotsBeforeDecimal++ ;
   }

   if ( expr4infoPtrLen(expr4infoPtr) < numSpotsBeforeDecimal )  // means not enough room for number...
   {
      // overflow, fill with asterisks
      c4memset( ptr, '*', expr4infoPtrLen(expr4infoPtr) ) ;
   }
   else
   {
      int numDecimals = expr4infoPtrLen(expr4infoPtr) - numSpotsBeforeDecimal ;
      if ( numDecimals > 0 )  // leave room for '.' sign as well...
         numDecimals-- ;
      #ifdef S4DATA_ALIG2
         memcpy( (void *)&doub, expr4[-1], sizeof(double) );
         c4dtoa45( doub, ptr, expr4infoPtrLen(expr4infoPtr), numDecimals ) ;
      #else
         c4dtoa45( *(double *) expr4[-1], ptr, expr4infoPtrLen(expr4infoPtr), numDecimals ) ;
      #endif
   }

   Bool5 rightJustify = 1 ;   // true if we need to right justify the string (i.e. 0s at end after decimal)
   Bool5 doneCountingZeros = 0 ;  // true when we hit the last of the decimals at the right
   // now, count the # of '0's after the decimal point at the end of string...
   int numZerosAtRight = 0 ;  // # of zeros to process...
   for ( int strIndex = expr4infoPtrLen(expr4infoPtr) - 1 ;; strIndex-- )
   {
      // check for '.' before strIndex 0
      if ( ptr[strIndex] == '.' ) // found decimal point, done...
      {
         // if we are still counting zeros, add 1 to eliminate this trailing decimal point...
         if ( doneCountingZeros == 0 )
            numZerosAtRight++ ;
         break ;
      }
      if ( strIndex == 0 )  // means went through string, and no '.', so no copy to do...
      {
         rightJustify = 0 ;
         break ;
      }
      if ( ptr[strIndex] != '0' ) // not '0', done...but still look for decimal, if none, no right justification
         doneCountingZeros = 1 ;
      else
         if ( doneCountingZeros == 0 )
         {
            // if here, then we have a '0'. increment numZerosAtRight
            numZerosAtRight++ ;
         }
   }

   if ( rightJustify == 1 && numZerosAtRight > 0 ) // 0's at right, and after a decimal point...
   {
      // all we need to do is move the string over and then insert '0's...
      c4memmove( ptr + numZerosAtRight, ptr, expr4infoPtrLen(expr4infoPtr) - numZerosAtRight ) ;
      c4memset( ptr, fillerChar, numZerosAtRight ) ;
   }
   else // we still need to replace any blanks on the right with 0's
   {
      for ( int strIndex = 0 ; strIndex < expr4infoPtrLen(expr4infoPtr) ; strIndex++ )
      {
         if ( ptr[strIndex] != ' ' )  // done
            break ;
         ptr[strIndex] = fillerChar ;  // replace blank with the filler character
      }
   }

   expr4[-1] = ptr ;
*/
}



void e4str()
{
   char *ptr ;
   #ifdef S4DATA_ALIG2
        double doub;
   #endif

   ptr = expr4buf + expr4infoPtr->resultPos ;
   #ifdef S4DATA_ALIG2
      memcpy( (void *)&doub, expr4[-1], sizeof(double) );
      c4dtoa45( doub, ptr, expr4infoPtrLen(expr4infoPtr), expr4infoPtr->i1 ) ;
   #else
      c4dtoa45( *(double *) expr4[-1], ptr, expr4infoPtrLen(expr4infoPtr), expr4infoPtr->i1 ) ;
   #endif
   expr4[-1] = ptr ;
}



void e4wideToStr()
{
   char *ptr ;

   ptr = expr4buf + expr4infoPtr->resultPos ;
   #ifdef S4WINCE
      WSTR5 *tempBuff = (WSTR5 *)u4alloc( expr4infoPtrLen(expr4infoPtr) * sizeof(unsigned short) ) ;
      memcpy( tempBuff, expr4[-1], expr4infoPtrLen(expr4infoPtr) * sizeof(unsigned short) ) ;
      c4utoaLen( tempBuff, ptr, expr4infoPtrLen(expr4infoPtr) ) ;
      u4free( tempBuff ) ;
   #else
      #ifdef S4WIN32
         // AS Aug 1/08 - change to use the Ansi code page since if you have foreign characters they otherwise don't get converted well
         WideCharToMultiByte( CP_ACP, 0, (LPCWSTR)expr4[-1], expr4infoPtrLen(expr4infoPtr), ptr, expr4infoPtrLen(expr4infoPtr), NULL, NULL ) ;
         //WideCharToMultiByte( CP_OEMCP, 0, (LPCWSTR)expr4[-1], expr4infoPtrLen(expr4infoPtr), ptr, expr4infoPtrLen(expr4infoPtr), NULL, NULL ) ;
      #else
         /* no widestring conversion available, just memcpy */
         c4memcpy( ptr, expr4[-1], expr4infoPtrLen(expr4infoPtr) ) ;
      #endif
   #endif
   expr4[-1] = ptr ;
}



void e4sub()
{
   double *doublePtr ;
   #ifdef S4DATA_ALIG2
      double a,b;
   #endif

   doublePtr = (double *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr + firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *doublePtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            memcpy(&a, expr4[-2], sizeof(double));
            memcpy(&b, expr4[-1], sizeof(double));
            *doublePtr = a-b;
         #else
            *doublePtr = *(double *)expr4[-2] - *(double *)expr4[-1] ;
         #endif
      }

   expr4[-2] = (char *) doublePtr ;
   expr4-- ;
}



void e4subFloat()
{
   float *floatPtr ;
   #ifdef S4DATA_ALIG2
      float a,b;
   #endif

   floatPtr = (float *)(expr4buf + expr4infoPtr->resultPos) ;

   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr + firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *floatPtr = 0 ;
      }
      else
   #endif
      {
         #ifdef S4DATA_ALIG2
            assert5( 0 ) ;  // not supported
         #else
            *floatPtr = *(float *)expr4[-2] - *(float *)expr4[-1] ;
         #endif
      }

   expr4[-2] = (char *) floatPtr ;
   expr4-- ;
}



void e4subDate()
{
   #ifdef S4CLIENT_OR_FOX
      // AS Feb 20/04 - support for r4null handling
      int firstEntry = -1 ;
      int secondEntry = firstEntry - (expr4infoPtr+firstEntry)->numEntries ;
      if ( (expr4infoPtr + firstEntry)->isNull || (expr4infoPtr+secondEntry)->isNull )
      {
         expr4infoPtr->isNull = 1 ;
         *(double *)expr4-- = 0.0 ;
         return ;
      }
      else
   #endif
      {
         if ( v4functions[expr4infoPtr->functionI].type[0] == r4dateDoub )
         {
            if ( *(double *)expr4[-2] == 0.0 )
            {
               *(double *)expr4-- = 0.0 ;
               return ;
            }
         }

         if ( v4functions[expr4infoPtr->functionI].type[1] == r4dateDoub )
         {
            if ( *(double *)expr4[-1] == 0.0 )
            {
               *(double *)expr4-- = 0.0 ;
               return ;
            }
         }
   }

   e4sub() ;
}



void e4substr()
{
   // AS Dec 15/04 - support for subexpressions...if the first (string) paramater is varaible, then use that length...
   // if ( (expr4infoPtr-1)->varLength == 1 && expr4infoPtr->functionI == E4RIGHT)
   if ( expr4infoPtr->functionI == E4RIGHT)
   {
      // in this case we will have detected that we were variable, and the stored i1 value indicates the number of characters on the
      // right to use...
      int varLength = (expr4infoPtrLen(expr4infoPtr-1)) ;
      int numShift ;
      if ( (expr4infoPtr-1)->varLength == 1 )   // if variable, we need to take the variable length into account...
         numShift = max( 0, varLength - expr4infoPtr->i1 ) ;
      else  // otherwise just use the i1 setting...
         numShift = expr4infoPtr->i1 ;
      c4memmove( expr4buf + expr4infoPtr->resultPos, expr4buf + expr4infoPtr->resultPos + numShift, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;
   }
   else
      c4memmove( expr4buf + expr4infoPtr->resultPos, expr4buf + expr4infoPtr->resultPos + expr4infoPtr->i1, (unsigned int)expr4infoPtrLen(expr4infoPtr) ) ;

   expr4infoPtr->varLength = 1 ;
   expr4infoPtr->i2 = expr4infoPtr->len ;
}



void e4time()
{
   date4timeNow( *expr4++ = expr4buf + expr4infoPtr->resultPos ) ;
}



void e4trim()
{
   // AS Dec 15/04 - support for subexpressions...
   expr4infoPtr->i2 = expr4infoPtr->len - c4trimNLow( expr4[-1], expr4infoPtr->len + 1 ) ;
   expr4infoPtr->varLength = 1 ;
}



void expr4trueFunction()
{
   int *ptr ;
   ptr = (int *)(*expr4++ = expr4buf+expr4infoPtr->resultPos) ;
   *ptr = 1 ;
}



void e4upper()
{
   expr4[-1][expr4infoPtrLen(expr4infoPtr)] = 0 ;
   // AS Jun 17/03 - Also applies to CLIENT if fox
   #if defined(S4CLIENT_OR_FOX) && ( defined(S4WIN32) || defined(S4WIN16) )
      DATA4FILE *dataFile = expr4ptr->data->dataFile ;
      if ( code4indexFormat( dataFile->c4 ) == r4cdx )
      {
         // AS 02/15/00 FoxPro compatibility, if using the windows ansi code page, should be using windows ansi upper,
         // not c4upper which is a machine upper...
         // AS Dec 30/02 - support for CodePage 1250 - treat same as 1252 for upper casing.
         if ( dataFile->codePage == cp1252 || dataFile->codePage == cp1250 )
         {
            #ifdef S4WIN16
               AnsiUpper( expr4[-1] ) ;
            #else
               #ifdef S4UNICODE  // CS 2001/06/22
                  // LY Jul 2/04 : replaced c4upper(), since fails to convert accented characters correctly
                  WSTR5 uBuff[I4MAX_KEY_SIZE+1] ;
                  c4atou( expr4[-1], uBuff, expr4infoPtrLen(expr4infoPtr) ) ;
                  CharUpper( uBuff ) ;
                  c4utoa( uBuff ) ;
                  memcpy( expr4[-1], uBuff, expr4infoPtrLen(expr4infoPtr) ) ;
               #else
                  CharUpper( expr4[-1] ) ;
               #endif
            #endif
            return ;
         }
      }
      // otherwise just fall through to regular upper casing
   #endif
   c4upper( expr4[-1] ) ;
}



#ifndef S4UNIX
   void e4l2bin()
   {
      // for non-UNIX only...
      // clipper L2BIN() function, converts an input numeric value (in character format, or numeric field)
      // to a 4-byute binary long integer value.
      char *ptr = expr4buf + expr4infoPtr->resultPos ;
      long result = (long)(*((double *)expr4[-1] )) ;
      c4memcpy( ptr, (void *)&result, sizeof(result) ) ;
      expr4[-1] = (char *)ptr ;
   }
#endif /* !S4UNIX */



void e4val()
{
   char *ptr ;
   double d ;

   ptr = expr4buf + expr4infoPtr->resultPos ;
   d = c4atod( expr4[-1], expr4infoPtr[-1].len ) ;
   #ifdef E4ANALYZE
      if ( ptr == 0 )
         error4( 0, e4info, E80903 ) ;
   #endif
   c4memcpy( ptr, (void *)&d, sizeof(d) ) ;
   expr4[-1] = (char *) ptr ;
}



void e4year()
{
   double d ;

   d = (double) date4year( expr4[-1] ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   *(double *)expr4[-1] = d ;
}



void e4yearDoub()
{
   char datePbuf[8] ;

   date4assign( datePbuf, (long) *(double *)expr4[-1] ) ;
   expr4[-1] = expr4buf + expr4infoPtr->resultPos ;
   *(double *) expr4[-1] = date4year( datePbuf ) ;
}



void e4ascend()
{
   double d ;
   char *ptr ;

   /* modified 09/15/95 AS -- need to always perform ascend in the expression
      buffer in case of concatanations */
   ptr = expr4buf + expr4infoPtr->resultPos ;
   if ( expr4[-1] != ptr )
   {
      c4memcpy( ptr, expr4[-1], expr4infoPtrLen(expr4infoPtr) ) ;
      expr4[-1] = ptr ;
   }

   int keyType = v4functions[expr4infoPtr->functionI].type[0] ;

   switch( keyType )
   {
      case r4num:
         e4applyAscend( keyType, expr4[-1], expr4infoPtrLen(expr4infoPtr) ) ;
         break ;
      case r4dateDoub:
         #ifdef S4DATA_ALIGN  /* LY 2001/04/18 */
            memcpy( &d, (double *)expr4[-1], sizeof(double) ) ;
         #else
            d = *(double *)expr4[-1] ;
         #endif
         e4applyAscend( keyType, expr4[-1], (unsigned long)d ) ;
         break ;
      case r4log:
         e4applyAscend( keyType, expr4[-1], 0 ) ;
         break ;
      #ifdef S4FOX
         case r4numDoub:
            e4applyAscend( keyType, expr4[-1], 0 ) ;
            break ;
         case r5i2:
            {
               long i2asInt = *((short *)expr4[-1]) ;
               e4applyAscend( keyType, expr4[-1], i2asInt ) ;
            }
            break ;
         case r5ui2:
            {
               unsigned long ui2asUnsignedLong = *((unsigned short *)expr4[-1]) ;
               e4applyAscend( keyType, expr4[-1], ui2asUnsignedLong ) ;
            }
            break ;
         case r5ui4:
            e4applyAscend( keyType, expr4[-1], *((unsigned long *)expr4[-1]) ) ;
            break ;
         case r4int:
            e4applyAscend( keyType, expr4[-1], *((unsigned long *)expr4[-1]) ) ;
            break ;
         #ifndef S4NO_LONGLONG
            case r5i8:
               t4i8ToFox( expr4[-1], ((LONGLONG *)expr4[-1]) ) ;
               break ;
         #endif
         case r5dbDate:
         case r5dbTime:
         case r5dbTimeStamp:
         case r4currency:
         case r4dateTime:
         case r4dateTimeMilli:  // AS Mar 10/03 - ms support in datetic
            e4applyAscend( keyType, expr4[-1], 0 ) ;
            break ;
      #endif /* S4FOX */
      case r5wstr:
      case r5wstrLen:
         // AS 08/09/99 Ascend will always collate the unicode as machine sequence... (i.e. reverse short on chars)
         e4applyAscend( keyType, expr4[-1], expr4infoPtrLen(expr4infoPtr) ) ;
         break ;
      // AS May 10/06 - if necessary, apply the collating sequence to this part of the string
      case r4str:
         #ifdef S4FOX
            {
               TAG4FILE *t4file = expr4infoPtr->tagPtr ;
               if ( t4file != 0 )
               {
                  COLLATE4 *collate = collation4get( t4file->collateName ) ;
                  if ( collate->collateType != collate4machineByteOrder )
                  {
                     int lenOut ;
                     int lenIn = expr4infoPtrLen(expr4infoPtr) / (1+collate->keySizeCharPerCharAdd) ;
                     if ( collate4convertCharToKey( collate, t4file->expr->codeBase->storedKey, expr4[-1], lenIn, t4file->isUnicode, &lenOut ) < 0 )
                        return ;
                     assert5( lenOut <= t4file->expr->codeBase->storedKeyLen ) ;
                     memcpy( expr4[-1], t4file->expr->codeBase->storedKey, lenOut ) ;
                  }
               }
            }
         #endif
      default:  /* r4date */
         /* in these instances, already in ascending order, so don't need to do anything */
         break ;
   }

}



#ifdef S4CLIPPER
   void e4descendBinary()
   {
      e4ascend() ;
      c4descendBinary( expr4[-1], expr4[-1], expr4infoPtrLen(expr4infoPtr) ) ;
   }
#endif



void e4descend()
{
   e4ascend() ;
   c4descend( expr4[-1], expr4[-1], expr4infoPtrLen(expr4infoPtr) ) ;
}



void e4calcFunction()
{
   EXPR4CALC *e4calcPtr = (EXPR4CALC *) expr4infoPtr->p1 ;
   char **e4save = expr4 ;
   char *expr4constantsSave = expr4constants ;
   char *resultPtr ;
   EXPR4 *expr4ptrSave = expr4ptr ;

   // AS May 31/04 - may return an out of memory failure now...
   if ( expr4calcResultPos( e4calcPtr, expr4infoPtr->resultPos ) != 0 )
      return ;
   expr4vary( e4calcPtr->expr, &resultPtr ) ;
   expr4reset( e4calcPtr->expr ) ;  /* restart from vary */

   expr4 = e4save ;
   expr4constants = expr4constantsSave ;
   expr4ptr = expr4ptrSave ;
   *expr4++ = resultPtr ;
}


// AS Jun 27/03 - Added EMPTY() function - for now for r4str types only
void e4empty( void )
{
   int result = 1 ;

   // AS Dec 5/05 - fix for OFF_MEMO
   #ifndef S4OFF_MEMO
      // AS Jun 27/03 - special case for memo fields...
      FIELD4 *mFld = expr4infoPtr[-1].fieldPtr ;
      if ( mFld != 0 && mFld->memo != 0 )
      {
         // in this case look at the data file memo value itself (is it 0 or blank?)
         long val = f4long( mFld ) ;
         if ( val != 0 )
            result = 0 ;
      }
      else
   #endif
   {
      char *fldPtr = expr4[-1] ;
      int len = (expr4infoPtr-1)->len ;

      for ( int loop = 0 ; loop < len ; loop++ )
      {
         if ( fldPtr[loop] != ' ' )
         {
            result = 0 ;
            break ;
         }
      }
   }

   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   *(int *)(ptr) = result ;
   expr4[-1] = ptr ;
}



void e4emptyNum( void )
{
   int result = 1 ;
   double val = *(double *)expr4[-1] ;
   if ( val != 0 )
      result = 0 ;

   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   *(int *)(ptr) = result ;
   expr4[-1] = ptr ;
}



void e4emptyDtTm( void )
{
   int result = 1 ;
   #ifdef S4DATA_ALIGN
      if ( memcmp( expr4[-1], "\0\0\0\0", sizeof(S4LONG) ) )
   #else
      S4LONG *val = (S4LONG *)expr4[-1] ;
      if ( *val != 0 )
   #endif
      result = 0 ;

   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   *(int *)(ptr) = result ;
   expr4[-1] = ptr ;
}



void e4emptyLog( void )
{
   int result = 1 ;
   int *val = (int *)expr4[-1] ;
   if ( *val != 0 )
      result = 0 ;

   char *ptr = expr4buf + expr4infoPtr->resultPos ;
   *(int *)(ptr) = result ;
   expr4[-1] = ptr ;
}



#ifndef S4OFF_REPORT
   void e4pageno()
   {
      double d = (double) expr4ptr->codeBase->pageno ;
      memcpy( *expr4++ = expr4buf + expr4infoPtr->resultPos, (void *)&d, sizeof(d) ) ;
   }



   void e4calcTotal()
   {
      double d = total4value( (struct TOTAL4st *) expr4infoPtr->p1 ) ;
      *expr4 = expr4buf + expr4infoPtr->resultPos ;
      memcpy( *expr4++, (void *)&d, sizeof(d) ) ;
   }



   double S4FUNCTION total4value( TOTAL4 *t4 )
   {
      switch( t4->totalType )
      {
         case total4sum:
            return t4->total ;
         case total4count:
            return (double) t4->count ;
         case total4average:
            if( t4->count == 0 )
               return 0.0 ;
            return t4->total/t4->count ;
         case total4highest:
            return t4->high ;
         case total4lowest:
            return t4->low ;
         default:
            break ;
      }

      return t4->total ;
   }
#endif /* !S4OFF_REPORT */
