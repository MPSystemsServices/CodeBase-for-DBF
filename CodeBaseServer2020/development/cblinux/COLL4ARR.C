/* coll4arr.c (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved. */

/* contains arrays for FoxPro various collations we support in-code. */

/* included by i4conv.c */

static Translate4arrayChar cp1252generalCollationArray[] =
{
   /* head, tail .... comments:  <actual unsigned character: '≤' = not displayable > <array entry # / ascii code #) */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   0 - 0x00 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   1 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   2 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   3 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   4 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   5 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   6 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   7 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   8 */
   { (unsigned char) 17,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   9 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  10 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  11 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  12 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  13 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  14 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  15 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  16 - 0x10 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  17 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  18 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  19 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  20 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  21 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  22 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  23 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  24 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  25 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  26 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  27 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  28 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  29 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  30 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  31 */
   { (unsigned char) 17,                      (unsigned char)NO4TAIL_BYTES },  /*    32 - 0x20 */
   { (unsigned char) 18,                      (unsigned char)NO4TAIL_BYTES },  /* !  33 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* "  34 */
   { (unsigned char) 20,                      (unsigned char)NO4TAIL_BYTES },  /* #  35 */
   { (unsigned char) 21,                      (unsigned char)NO4TAIL_BYTES },  /* $  36 */
   { (unsigned char) 22,                      (unsigned char)NO4TAIL_BYTES },  /* %  37 */
   { (unsigned char) 23,                      (unsigned char)NO4TAIL_BYTES },  /* &  38 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* '  39 */
   { (unsigned char) 25,                      (unsigned char)NO4TAIL_BYTES },  /* (  40 */
   { (unsigned char) 26,                      (unsigned char)NO4TAIL_BYTES },  /* )  41 */
   { (unsigned char) 27,                      (unsigned char)NO4TAIL_BYTES },  /* *  42 */
   { (unsigned char) 28,                      (unsigned char)NO4TAIL_BYTES },  /* +  43 */
   { (unsigned char) 29,                      (unsigned char)NO4TAIL_BYTES },  /* ,  44 */
   { (unsigned char) 30,                      (unsigned char)NO4TAIL_BYTES },  /* -  45 */
   { (unsigned char) 31,                      (unsigned char)NO4TAIL_BYTES },  /* .  46 */
   { (unsigned char) 32,                      (unsigned char)NO4TAIL_BYTES },  /* /  47 */
   { (unsigned char) 86,                      (unsigned char)NO4TAIL_BYTES },  /* 0  48 - 0x30 */
   { (unsigned char) 87,                      (unsigned char)NO4TAIL_BYTES },  /* 1  49 */
   { (unsigned char) 88,                      (unsigned char)NO4TAIL_BYTES },  /* 2  50 */
   { (unsigned char) 89,                      (unsigned char)NO4TAIL_BYTES },  /* 3  51 */
   { (unsigned char) 90,                      (unsigned char)NO4TAIL_BYTES },  /* 4  52 */
   { (unsigned char) 91,                      (unsigned char)NO4TAIL_BYTES },  /* 5  53 */
   { (unsigned char) 92,                      (unsigned char)NO4TAIL_BYTES },  /* 6  54 */
   { (unsigned char) 93,                      (unsigned char)NO4TAIL_BYTES },  /* 7  55 */
   { (unsigned char) 94,                      (unsigned char)NO4TAIL_BYTES },  /* 8  56 */
   { (unsigned char) 95,                      (unsigned char)NO4TAIL_BYTES },  /* 9  57 */
   { (unsigned char) 33,                      (unsigned char)NO4TAIL_BYTES },  /* :  58 */
   { (unsigned char) 34,                      (unsigned char)NO4TAIL_BYTES },  /* ;  59 */
   { (unsigned char) 35,                      (unsigned char)NO4TAIL_BYTES },  /* <  60 */
   { (unsigned char) 36,                      (unsigned char)NO4TAIL_BYTES },  /* =  61 */
   { (unsigned char) 37,                      (unsigned char)NO4TAIL_BYTES },  /* >  62 */
   { (unsigned char) 38,                      (unsigned char)NO4TAIL_BYTES },  /* ?  63 */
   { (unsigned char) 39,                      (unsigned char)NO4TAIL_BYTES },  /* @  64 - 0x40 */
   { (unsigned char) 96,                      (unsigned char)0             },  /* A  65 */
   { (unsigned char) 97,                      (unsigned char)NO4TAIL_BYTES },  /* B  66 */
   { (unsigned char) 98,                      (unsigned char)0             },  /* C  67 */
   { (unsigned char)100,                      (unsigned char)NO4TAIL_BYTES },  /* D  68 */
   { (unsigned char)102,                      (unsigned char)0             },  /* E  69 */
   { (unsigned char)103,                      (unsigned char)NO4TAIL_BYTES },  /* F  70 */
   { (unsigned char)104,                      (unsigned char)NO4TAIL_BYTES },  /* G  71 */
   { (unsigned char)105,                      (unsigned char)NO4TAIL_BYTES },  /* H  72 */
   { (unsigned char)106,                      (unsigned char)0             },  /* I  73 */
   { (unsigned char)107,                      (unsigned char)NO4TAIL_BYTES },  /* J  74 */
   { (unsigned char)108,                      (unsigned char)NO4TAIL_BYTES },  /* K  75 */
   { (unsigned char)109,                      (unsigned char)NO4TAIL_BYTES },  /* L  76 */
   { (unsigned char)111,                      (unsigned char)NO4TAIL_BYTES },  /* M  77 */
   { (unsigned char)112,                      (unsigned char)0             },  /* N  78 */
   { (unsigned char)114,                      (unsigned char)0             },  /* O  79 */
   { (unsigned char)115,                      (unsigned char)NO4TAIL_BYTES },  /* P  80 - 0x50 */
   { (unsigned char)116,                      (unsigned char)NO4TAIL_BYTES },  /* Q  81 */
   { (unsigned char)117,                      (unsigned char)NO4TAIL_BYTES },  /* R  82 */
   { (unsigned char)118,                      (unsigned char)0             },  /* S  83 */
   { (unsigned char)119,                      (unsigned char)NO4TAIL_BYTES },  /* T  84 */
   { (unsigned char)120,                      (unsigned char)0             },  /* U  85 */
   { (unsigned char)122,                      (unsigned char)NO4TAIL_BYTES },  /* V  86 */
   { (unsigned char)123,                      (unsigned char)NO4TAIL_BYTES },  /* W  87 */
   { (unsigned char)124,                      (unsigned char)NO4TAIL_BYTES },  /* X  88 */
   { (unsigned char)125,                      (unsigned char)0             },  /* Y  89 */
   { (unsigned char)126,                      (unsigned char)NO4TAIL_BYTES },  /* Z  90 */
   { (unsigned char) 40,                      (unsigned char)NO4TAIL_BYTES },  /* [  91 */
   { (unsigned char) 41,                      (unsigned char)NO4TAIL_BYTES },  /* \  92 */
   { (unsigned char) 42,                      (unsigned char)NO4TAIL_BYTES },  /* ]  93 */
   { (unsigned char) 43,                      (unsigned char)NO4TAIL_BYTES },  /* ^  94 */
   { (unsigned char) 44,                      (unsigned char)NO4TAIL_BYTES },  /* _  95 */
   { (unsigned char) 45,                      (unsigned char)NO4TAIL_BYTES },  /* `  96 - 0x60 */
   { (unsigned char) 96,                      (unsigned char)0             },  /* a  97 */
   { (unsigned char) 97,                      (unsigned char)NO4TAIL_BYTES },  /* b  98 */
   { (unsigned char) 98,                      (unsigned char)0             },  /* c  99 */
   { (unsigned char)100,                      (unsigned char)NO4TAIL_BYTES },  /* d 100 */
   { (unsigned char)102,                      (unsigned char)0             },  /* e 101 */
   { (unsigned char)103,                      (unsigned char)NO4TAIL_BYTES },  /* f 102 */
   { (unsigned char)104,                      (unsigned char)NO4TAIL_BYTES },  /* g 103 */
   { (unsigned char)105,                      (unsigned char)NO4TAIL_BYTES },  /* h 104 */
   { (unsigned char)106,                      (unsigned char)0             },  /* i 105 */
   { (unsigned char)107,                      (unsigned char)NO4TAIL_BYTES },  /* j 106 */
   { (unsigned char)108,                      (unsigned char)NO4TAIL_BYTES },  /* k 107 */
   { (unsigned char)109,                      (unsigned char)NO4TAIL_BYTES },  /* l 108 */
   { (unsigned char)111,                      (unsigned char)NO4TAIL_BYTES },  /* m 109 */
   { (unsigned char)112,                      (unsigned char)0             },  /* n 110 */
   { (unsigned char)114,                      (unsigned char)0             },  /* o 111 */
   { (unsigned char)115,                      (unsigned char)NO4TAIL_BYTES },  /* p 112 - 0x70 */
   { (unsigned char)116,                      (unsigned char)NO4TAIL_BYTES },  /* q 113 */
   { (unsigned char)117,                      (unsigned char)NO4TAIL_BYTES },  /* r 114 */
   { (unsigned char)118,                      (unsigned char)0             },  /* s 115 */
   { (unsigned char)119,                      (unsigned char)NO4TAIL_BYTES },  /* t 116 */
   { (unsigned char)120,                      (unsigned char)0             },  /* u 117 */
   { (unsigned char)122,                      (unsigned char)NO4TAIL_BYTES },  /* v 118 */
   { (unsigned char)123,                      (unsigned char)NO4TAIL_BYTES },  /* w 119 */
   { (unsigned char)124,                      (unsigned char)NO4TAIL_BYTES },  /* x 120 */
   { (unsigned char)125,                      (unsigned char)0             },  /* y 121 */
   { (unsigned char)126,                      (unsigned char)NO4TAIL_BYTES },  /* z 122 */
   { (unsigned char) 46,                      (unsigned char)NO4TAIL_BYTES },  /* { 123 */
   { (unsigned char) 47,                      (unsigned char)NO4TAIL_BYTES },  /* | 124 */
   { (unsigned char) 48,                      (unsigned char)NO4TAIL_BYTES },  /* } 125 */
   { (unsigned char) 49,                      (unsigned char)NO4TAIL_BYTES },  /*   126 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 127 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 128 - 0x80 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 129 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 130 */
   { (unsigned char) 50,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 131 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 132 */
   { (unsigned char) 51,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 133 */
   { (unsigned char) 52,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 134 */
   { (unsigned char) 53,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 135 */
   { (unsigned char) 54,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 136 */
   { (unsigned char) 55,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 137 */
   { (unsigned char)118,                      (unsigned char)8             },  /* ≤ 138 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 139 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)0             },  /* ≤ 140 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 141 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 142 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 143 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 144 - 0x90 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* ` 145 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* ' 146 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 147 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 148 */
   { (unsigned char) 56,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 149 */
   { (unsigned char) 30,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 150 */
   { (unsigned char) 30,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 151 */
   { (unsigned char) 57,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 152 */
   { (unsigned char) 58,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 153 */
   { (unsigned char)118,                      (unsigned char)8             },  /* ≤ 154 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 155 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)0             },  /* ≤ 156 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 157 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 158 */
   { (unsigned char)125,                      (unsigned char)4             },  /* ≤ 159 */
   { (unsigned char) 32,                      (unsigned char)1             },  /*   160 - 0xA0 */
   { (unsigned char) 59,                      (unsigned char)NO4TAIL_BYTES },  /* ≠ 161 */
   { (unsigned char) 60,                      (unsigned char)NO4TAIL_BYTES },  /* õ 162 */
   { (unsigned char) 61,                      (unsigned char)NO4TAIL_BYTES },  /* ú 163 */
   { (unsigned char) 62,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 164 */
   { (unsigned char) 63,                      (unsigned char)NO4TAIL_BYTES },  /* ù 165 */
   { (unsigned char) 64,                      (unsigned char)NO4TAIL_BYTES },  /* | 166 */
   { (unsigned char) 65,                      (unsigned char)NO4TAIL_BYTES },  /*  167 */
   { (unsigned char) 66,                      (unsigned char)NO4TAIL_BYTES },  /* " 168 */
   { (unsigned char) 67,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 169 */
   { (unsigned char) 68,                      (unsigned char)NO4TAIL_BYTES },  /* ¶ 170 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* Æ 171 */
   { (unsigned char) 69,                      (unsigned char)NO4TAIL_BYTES },  /* ™ 172 */
   { (unsigned char) 30,                      (unsigned char)NO4TAIL_BYTES },  /* - 173 */
   { (unsigned char) 70,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 174 */
   { (unsigned char) 71,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 175 */
   { (unsigned char) 72,                      (unsigned char)NO4TAIL_BYTES },  /* ¯ 176 - 0xB0 */
   { (unsigned char) 73,                      (unsigned char)NO4TAIL_BYTES },  /* Ò 177 */
   { (unsigned char) 88,                      (unsigned char)NO4TAIL_BYTES },  /* ˝ 178 */
   { (unsigned char) 89,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 179 */
   { (unsigned char) 74,                      (unsigned char)NO4TAIL_BYTES },  /* ' 180 */
   { (unsigned char) 75,                      (unsigned char)NO4TAIL_BYTES },  /* Ê 181 */
   { (unsigned char) 76,                      (unsigned char)NO4TAIL_BYTES },  /*  182 */
   { (unsigned char) 77,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 183 */
   { (unsigned char) 78,                      (unsigned char)NO4TAIL_BYTES },  /* , 184 */
   { (unsigned char) 87,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 185 */
   { (unsigned char) 79,                      (unsigned char)NO4TAIL_BYTES },  /* ß 186 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* Ø 187 */
   { (unsigned char) 80,                      (unsigned char)NO4TAIL_BYTES },  /* ¨ 188 */
   { (unsigned char) 81,                      (unsigned char)NO4TAIL_BYTES },  /* ´ 189 */
   { (unsigned char) 82,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 190 */
   { (unsigned char) 83,                      (unsigned char)NO4TAIL_BYTES },  /* ® 191 */
   { (unsigned char) 96,                      (unsigned char)2             },  /* A 192 - 0xC0 */
   { (unsigned char) 96,                      (unsigned char)1             },  /* A 193 */
   { (unsigned char) 96,                      (unsigned char)3             },  /* A 194 */
   { (unsigned char) 96,                      (unsigned char)5             },  /* A 195 */
   { (unsigned char) 96,                      (unsigned char)4             },  /* é 196 */
   { (unsigned char) 96,                      (unsigned char)6             },  /* è 197 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)1             },  /* í 198 */
   { (unsigned char) 98,                      (unsigned char)7             },  /* Ä 199 */
   { (unsigned char)102,                      (unsigned char)2             },  /* E 200 */
   { (unsigned char)102,                      (unsigned char)1             },  /* ê 201 */
   { (unsigned char)102,                      (unsigned char)3             },  /* E 202 */
   { (unsigned char)102,                      (unsigned char)4             },  /* E 203 */
   { (unsigned char)106,                      (unsigned char)2             },  /* I 204 */
   { (unsigned char)106,                      (unsigned char)1             },  /* I 205 */
   { (unsigned char)106,                      (unsigned char)3             },  /* I 206 */
   { (unsigned char)106,                      (unsigned char)4             },  /* I 207 */
   { (unsigned char)101,                      (unsigned char)NO4TAIL_BYTES },  /* D 208 - 0xD0 */
   { (unsigned char)112,                      (unsigned char)5             },  /* • 209 */
   { (unsigned char)114,                      (unsigned char)2             },  /* O 210 */
   { (unsigned char)114,                      (unsigned char)1             },  /* O 211 */
   { (unsigned char)114,                      (unsigned char)3             },  /* O 212 */
   { (unsigned char)114,                      (unsigned char)5             },  /* O 213 */
   { (unsigned char)114,                      (unsigned char)4             },  /* ô 214 */
   { (unsigned char) 84,                      (unsigned char)NO4TAIL_BYTES },  /* X 215 */
   { (unsigned char)129,                      (unsigned char)NO4TAIL_BYTES },  /* 0 216 */
   { (unsigned char)120,                      (unsigned char)2             },  /* U 217 */
   { (unsigned char)120,                      (unsigned char)1             },  /* U 218 */
   { (unsigned char)120,                      (unsigned char)3             },  /* U 219 */
   { (unsigned char)120,                      (unsigned char)4             },  /* ö 220 */
   { (unsigned char)125,                      (unsigned char)1             },  /* Y 221 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)2             },  /* b 222 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)3             },  /* · 223 */
   { (unsigned char) 96,                      (unsigned char)2             },  /* Ö 224 - 0xE0 */
   { (unsigned char) 96,                      (unsigned char)1             },  /* † 225 */
   { (unsigned char) 96,                      (unsigned char)3             },  /* É 226 */
   { (unsigned char) 96,                      (unsigned char)5             },  /* a 227 */
   { (unsigned char) 96,                      (unsigned char)4             },  /* Ñ 228 */
   { (unsigned char) 96,                      (unsigned char)6             },  /* Ü 229 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)1             },  /* ë 230 */
   { (unsigned char) 98,                      (unsigned char)7             },  /* á 231 */
   { (unsigned char)102,                      (unsigned char)2             },  /* ä 232 */
   { (unsigned char)102,                      (unsigned char)1             },  /* Ç 233 */
   { (unsigned char)102,                      (unsigned char)3             },  /* à 234 */
   { (unsigned char)102,                      (unsigned char)4             },  /* â 235 */
   { (unsigned char)106,                      (unsigned char)2             },  /* ç 236 */
   { (unsigned char)106,                      (unsigned char)1             },  /* ° 237 */
   { (unsigned char)106,                      (unsigned char)3             },  /* å 238 */
   { (unsigned char)106,                      (unsigned char)4             },  /* ã 239 */
   { (unsigned char)101,                      (unsigned char)NO4TAIL_BYTES },  /* Â 240 - 0xF0 */
   { (unsigned char)112,                      (unsigned char)5             },  /* § 241 */
   { (unsigned char)114,                      (unsigned char)2             },  /* ï 242 */
   { (unsigned char)114,                      (unsigned char)1             },  /* ¢ 243 */
   { (unsigned char)114,                      (unsigned char)3             },  /* ì 244 */
   { (unsigned char)114,                      (unsigned char)5             },  /* o 245 */
   { (unsigned char)114,                      (unsigned char)4             },  /* î 246 */
   { (unsigned char) 85,                      (unsigned char)NO4TAIL_BYTES },  /* ˆ 247 */
   { (unsigned char)129,                      (unsigned char)NO4TAIL_BYTES },  /* 0 248 */
   { (unsigned char)120,                      (unsigned char)2             },  /* ó 249 */
   { (unsigned char)120,                      (unsigned char)1             },  /* £ 250 */
   { (unsigned char)120,                      (unsigned char)3             },  /* ñ 251 */
   { (unsigned char)120,                      (unsigned char)4             },  /* Å 252 */
   { (unsigned char)125,                      (unsigned char)1             },  /* y 253 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)2             },  /* b 254 */
   { (unsigned char)125,                      (unsigned char)4             },  /* ò 255 */
} ;



static Expansion4compressionArray cp1252generalCompressArray[] =
{
   { expand4, { 79, 69 } },   /* "oe" */
   { expand4, { 65, 69 } },   /* "ae" */
   { expand4, { 84, 72 } },   /* "th" */
   { expand4, { 83, 83 } },   /* "ss" */
   { done4 },   // used to indicate reached end of array.
} ;



static Translate4arrayChar cp437generalCollationArray[] =
{
   /* head, tail .... comments:  <actual unsigned character: '≤' = not displayable > <array entry # / ascii code #) */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   0 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   1 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   2 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   3 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   4 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   5 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   6 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   7 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   8 */
   { (unsigned char) 17,                      (unsigned char)NO4TAIL_BYTES },  /* ≤   9 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  10 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  11 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  12 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  13 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  14 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  15 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  16 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  17 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  18 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  19 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  20 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  21 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  22 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  23 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  24 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  25 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  26 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  27 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  28 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  29 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  30 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤  31 */
   { (unsigned char) 17,                      (unsigned char)NO4TAIL_BYTES },  /*    32 */
   { (unsigned char) 18,                      (unsigned char)NO4TAIL_BYTES },  /* !  33 */
   { (unsigned char) 19,                      (unsigned char)NO4TAIL_BYTES },  /* "  34 */
   { (unsigned char) 20,                      (unsigned char)NO4TAIL_BYTES },  /* #  35 */
   { (unsigned char) 21,                      (unsigned char)NO4TAIL_BYTES },  /* $  36 */
   { (unsigned char) 22,                      (unsigned char)NO4TAIL_BYTES },  /* %  37 */
   { (unsigned char) 23,                      (unsigned char)NO4TAIL_BYTES },  /* &  38 */
   { (unsigned char) 24,                      (unsigned char)NO4TAIL_BYTES },  /* '  39 */
   { (unsigned char) 25,                      (unsigned char)NO4TAIL_BYTES },  /* (  40 */
   { (unsigned char) 26,                      (unsigned char)NO4TAIL_BYTES },  /* )  41 */
   { (unsigned char) 27,                      (unsigned char)NO4TAIL_BYTES },  /* *  42 */
   { (unsigned char) 28,                      (unsigned char)NO4TAIL_BYTES },  /* +  43 */
   { (unsigned char) 29,                      (unsigned char)NO4TAIL_BYTES },  /* ,  44 */
   { (unsigned char) 30,                      (unsigned char)NO4TAIL_BYTES },  /* -  45 */
   { (unsigned char) 31,                      (unsigned char)NO4TAIL_BYTES },  /* .  46 */
   { (unsigned char) 32,                      (unsigned char)NO4TAIL_BYTES },  /* /  47 */
   { (unsigned char) 86,                      (unsigned char)NO4TAIL_BYTES },  /* 0  48 */
   { (unsigned char) 87,                      (unsigned char)NO4TAIL_BYTES },  /* 1  49 */
   { (unsigned char) 88,                      (unsigned char)NO4TAIL_BYTES },  /* 2  50 */
   { (unsigned char) 89,                      (unsigned char)NO4TAIL_BYTES },  /* 3  51 */
   { (unsigned char) 90,                      (unsigned char)NO4TAIL_BYTES },  /* 4  52 */
   { (unsigned char) 91,                      (unsigned char)NO4TAIL_BYTES },  /* 5  53 */
   { (unsigned char) 92,                      (unsigned char)NO4TAIL_BYTES },  /* 6  54 */
   { (unsigned char) 93,                      (unsigned char)NO4TAIL_BYTES },  /* 7  55 */
   { (unsigned char) 94,                      (unsigned char)NO4TAIL_BYTES },  /* 8  56 */
   { (unsigned char) 95,                      (unsigned char)NO4TAIL_BYTES },  /* 9  57 */
   { (unsigned char) 33,                      (unsigned char)NO4TAIL_BYTES },  /* :  58 */
   { (unsigned char) 34,                      (unsigned char)NO4TAIL_BYTES },  /* ;  59 */
   { (unsigned char) 35,                      (unsigned char)NO4TAIL_BYTES },  /* <  60 */
   { (unsigned char) 36,                      (unsigned char)NO4TAIL_BYTES },  /* =  61 */
   { (unsigned char) 37,                      (unsigned char)NO4TAIL_BYTES },  /* >  62 */
   { (unsigned char) 38,                      (unsigned char)NO4TAIL_BYTES },  /* ?  63 */
   { (unsigned char) 39,                      (unsigned char)NO4TAIL_BYTES },  /* @  64 */
   { (unsigned char) 96,                      (unsigned char)0             },   /* A  65 */
   { (unsigned char) 97,                      (unsigned char)NO4TAIL_BYTES },  /* B  66 */
   { (unsigned char) 98,                      (unsigned char)0             },  /* C  67 */
   { (unsigned char)100,                      (unsigned char)NO4TAIL_BYTES },  /* D  68 */
   { (unsigned char)102,                      (unsigned char)0             },  /* E  69 */
   { (unsigned char)103,                      (unsigned char)NO4TAIL_BYTES },  /* F  70 */
   { (unsigned char)104,                      (unsigned char)NO4TAIL_BYTES },  /* G  71 */
   { (unsigned char)105,                      (unsigned char)NO4TAIL_BYTES },  /* H  72 */
   { (unsigned char)106,                      (unsigned char)0             },  /* I  73 */
   { (unsigned char)107,                      (unsigned char)NO4TAIL_BYTES },  /* J  74 */
   { (unsigned char)108,                      (unsigned char)NO4TAIL_BYTES },  /* K  75 */
   { (unsigned char)109,                      (unsigned char)NO4TAIL_BYTES },  /* L  76 */
   { (unsigned char)111,                      (unsigned char)NO4TAIL_BYTES },  /* M  77 */
   { (unsigned char)112,                      (unsigned char)0             },  /* N  78 */
   { (unsigned char)114,                      (unsigned char)0             },  /* O  79 */
   { (unsigned char)115,                      (unsigned char)NO4TAIL_BYTES },  /* P  80 */
   { (unsigned char)116,                      (unsigned char)NO4TAIL_BYTES },  /* Q  81 */
   { (unsigned char)117,                      (unsigned char)NO4TAIL_BYTES },  /* R  82 */
   { (unsigned char)118,                      (unsigned char)0             },  /* S  83 */
   { (unsigned char)119,                      (unsigned char)NO4TAIL_BYTES },  /* T  84 */
   { (unsigned char)120,                      (unsigned char)0             },  /* U  85 */
   { (unsigned char)122,                      (unsigned char)NO4TAIL_BYTES },  /* V  86 */
   { (unsigned char)123,                      (unsigned char)NO4TAIL_BYTES },  /* W  87 */
   { (unsigned char)124,                      (unsigned char)NO4TAIL_BYTES },  /* X  88 */
   { (unsigned char)125,                      (unsigned char)0             },  /* Y  89 */
   { (unsigned char)126,                      (unsigned char)NO4TAIL_BYTES },  /* Z  90 */
   { (unsigned char) 40,                      (unsigned char)NO4TAIL_BYTES },  /* [  91 */
   { (unsigned char) 41,                      (unsigned char)NO4TAIL_BYTES },  /* \  92 */
   { (unsigned char) 42,                      (unsigned char)NO4TAIL_BYTES },  /* ]  93 */
   { (unsigned char) 43,                      (unsigned char)NO4TAIL_BYTES },  /* ^  94 */
   { (unsigned char) 44,                      (unsigned char)NO4TAIL_BYTES },  /* _  95 */
   { (unsigned char) 45,                      (unsigned char)NO4TAIL_BYTES },  /* `  96 */
   { (unsigned char) 96,                      (unsigned char)0             },  /* a  97 */
   { (unsigned char) 97,                      (unsigned char)NO4TAIL_BYTES },  /* b  98 */
   { (unsigned char) 98,                      (unsigned char)0             },  /* c  99 */
   { (unsigned char)100,                      (unsigned char)NO4TAIL_BYTES },  /* d 100 */
   { (unsigned char)102,                      (unsigned char)0             },  /* e 101 */
   { (unsigned char)103,                      (unsigned char)NO4TAIL_BYTES },  /* f 102 */
   { (unsigned char)104,                      (unsigned char)NO4TAIL_BYTES },  /* g 103 */
   { (unsigned char)105,                      (unsigned char)NO4TAIL_BYTES },  /* h 104 */
   { (unsigned char)106,                      (unsigned char)0             },  /* i 105 */
   { (unsigned char)107,                      (unsigned char)NO4TAIL_BYTES },  /* j 106 */
   { (unsigned char)108,                      (unsigned char)NO4TAIL_BYTES },  /* k 107 */
   { (unsigned char)109,                      (unsigned char)NO4TAIL_BYTES },  /* l 108 */
   { (unsigned char)111,                      (unsigned char)NO4TAIL_BYTES },  /* m 109 */
   { (unsigned char)112,                      (unsigned char)0             },  /* n 110 */
   { (unsigned char)114,                      (unsigned char)0             },  /* o 111 */
   { (unsigned char)115,                      (unsigned char)NO4TAIL_BYTES },  /* p 112 */
   { (unsigned char)116,                      (unsigned char)NO4TAIL_BYTES },  /* q 113 */
   { (unsigned char)117,                      (unsigned char)NO4TAIL_BYTES },  /* r 114 */
   { (unsigned char)118,                      (unsigned char)0             },  /* s 115 */
   { (unsigned char)119,                      (unsigned char)NO4TAIL_BYTES },  /* t 116 */
   { (unsigned char)120,                      (unsigned char)0             },  /* u 117 */
   { (unsigned char)122,                      (unsigned char)NO4TAIL_BYTES },  /* v 118 */
   { (unsigned char)123,                      (unsigned char)NO4TAIL_BYTES },  /* w 119 */
   { (unsigned char)124,                      (unsigned char)NO4TAIL_BYTES },  /* x 120 */
   { (unsigned char)125,                      (unsigned char)0             },  /* y 121 */
   { (unsigned char)126,                      (unsigned char)NO4TAIL_BYTES },  /* z 122 */
   { (unsigned char) 46,                      (unsigned char)NO4TAIL_BYTES },  /* { (unsigned char)123 */
   { (unsigned char) 47,                      (unsigned char)NO4TAIL_BYTES },  /* | 124 */
   { (unsigned char) 48,                      (unsigned char)NO4TAIL_BYTES },  /* } 125 */
   { (unsigned char) 49,                      (unsigned char)NO4TAIL_BYTES },  /*   126 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 127 */
   { (unsigned char) 98,                      (unsigned char)7             },  /* Ä 128 */
   { (unsigned char)120,                      (unsigned char)4             },  /* Å 129 */
   { (unsigned char)102,                      (unsigned char)1             },  /* Ç 130 */
   { (unsigned char) 96,                      (unsigned char)3             },  /* É 131 */
   { (unsigned char) 96,                      (unsigned char)4             },  /* Ñ 132 */
   { (unsigned char) 96,                      (unsigned char)2             },  /* Ö 133 */
   { (unsigned char) 96,                      (unsigned char)6             },  /* Ü 134 */
   { (unsigned char) 98,                      (unsigned char)7             },  /* á 135 */
   { (unsigned char)102,                      (unsigned char)3             },  /* à 136 */
   { (unsigned char)102,                      (unsigned char)4             },  /* â 137 */
   { (unsigned char)102,                      (unsigned char)2             },  /* ä 138 */
   { (unsigned char)106,                      (unsigned char)4             },  /* ã 139 */
   { (unsigned char)106,                      (unsigned char)3             },  /* å 140 */
   { (unsigned char)106,                      (unsigned char)2             },  /* ç 141 */
   { (unsigned char) 96,                      (unsigned char)4             },  /* é 142 */
   { (unsigned char) 96,                      (unsigned char)6             },  /* è 143 */
   { (unsigned char)102,                      (unsigned char)1             },  /* ê 144 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)1             },  /* ë 145 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)1             },  /* í 146 */
   { (unsigned char)114,                      (unsigned char)3             },  /* ì 147 */
   { (unsigned char)114,                      (unsigned char)4             },  /* î 148 */
   { (unsigned char)114,                      (unsigned char)2             },  /* ï 149 */
   { (unsigned char)120,                      (unsigned char)3             },  /* ñ 150 */
   { (unsigned char)120,                      (unsigned char)2             },  /* ó 151 */
   { (unsigned char)125,                      (unsigned char)4             },  /* ò 152 */
   { (unsigned char)114,                      (unsigned char)4             },  /* ô 153 */
   { (unsigned char)120,                      (unsigned char)4             },  /* ö 154 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 155 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 156 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 157 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 158 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 159 */
   { (unsigned char) 96,                      (unsigned char)1             },  /* † 160 */
   { (unsigned char)106,                      (unsigned char)1             },  /* ° 161 */
   { (unsigned char)114,                      (unsigned char)1             },  /* ¢ 162 */
   { (unsigned char)120,                      (unsigned char)1             },  /* £ 163 */
   { (unsigned char)112,                      (unsigned char)5             },  /* § 164 */
   { (unsigned char)112,                      (unsigned char)5             },  /* • 165 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 166 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 167 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 168 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 169 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 170 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 171 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 172 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 173 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 174 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 175 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 176 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 177 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 178 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 179 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 180 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 181 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 182 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 183 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 184 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 185 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 186 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 187 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 188 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 189 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 190 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 191 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 192 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 193 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 194 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 195 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 196 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 197 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 198 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 199 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 200 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 201 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 202 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 203 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 204 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 205 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 206 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 207 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 208 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 209 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 210 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 211 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 212 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 213 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 214 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 215 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 216 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 217 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 218 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 219 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 220 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 221 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 222 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 223 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 224 */
   { (unsigned char)EXPAND4CHAR_TO_TWO_BYTES, (unsigned char)3             },  /* · 225*/
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 226 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 227 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 228 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 229 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 230 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 231 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 232 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 233 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 234 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 235 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 236 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 237 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 238 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 239 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 240 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 241 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 242 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 243 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 244 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 245 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 246 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 247 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 248 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 249 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 250 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 251 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 252 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 253 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 254 */
   { (unsigned char) 16,                      (unsigned char)NO4TAIL_BYTES },  /* ≤ 255 */
} ;



// Both CodePages are use the same compression array...
static Expansion4compressionArray *cp437generalCompressArray = cp1252generalCompressArray ;



// AS Dec 30/02 - Support for CodePage 1250, croation sorting
static Translate4arrayChar cp1250croatianCollationArray[] =
{
   /* head, tail */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   0 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   1 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   2 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   3 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   4 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   5 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   6 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   7 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   8 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   9 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  10 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  11 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  12 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  13 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  14 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  15 */
   { (unsigned char) 18, (unsigned char)NO4TAIL_BYTES },  /* ≤  16 */
   { (unsigned char) 19, (unsigned char)NO4TAIL_BYTES },  /* ≤  17 */
   { (unsigned char) 20, (unsigned char)NO4TAIL_BYTES },  /* ≤  18 */
   { (unsigned char) 21, (unsigned char)NO4TAIL_BYTES },  /* ≤  19 */
   { (unsigned char) 22, (unsigned char)NO4TAIL_BYTES },  /* ≤  20 */
   { (unsigned char) 23, (unsigned char)NO4TAIL_BYTES },  /* ≤  21 */
   { (unsigned char) 24, (unsigned char)NO4TAIL_BYTES },  /* ≤  22 */
   { (unsigned char) 25, (unsigned char)NO4TAIL_BYTES },  /* ≤  23 */
   { (unsigned char) 26, (unsigned char)NO4TAIL_BYTES },  /* ≤  24 */
   { (unsigned char) 27, (unsigned char)NO4TAIL_BYTES },  /* ≤  25 */
   { (unsigned char) 28, (unsigned char)NO4TAIL_BYTES },  /* ≤  26 */
   { (unsigned char) 29, (unsigned char)NO4TAIL_BYTES },  /* ≤  27 */
   { (unsigned char) 30, (unsigned char)NO4TAIL_BYTES },  /* ≤  28 */
   { (unsigned char) 31, (unsigned char)NO4TAIL_BYTES },  /* ≤  29 */
   { (unsigned char) 32, (unsigned char)NO4TAIL_BYTES },  /* ≤  30 */
   { (unsigned char) 33, (unsigned char)NO4TAIL_BYTES },  /* ≤  31 */
   { (unsigned char) 17, (unsigned char)NO4TAIL_BYTES },  /*    32 */
   { (unsigned char) 45, (unsigned char)NO4TAIL_BYTES },  /* !  33 */
   { (unsigned char) 46, (unsigned char)NO4TAIL_BYTES },  /* "  34 */
   { (unsigned char) 47, (unsigned char)NO4TAIL_BYTES },  /* #  35 */
   { (unsigned char) 48, (unsigned char)NO4TAIL_BYTES },  /* $  36 */
   { (unsigned char) 49, (unsigned char)NO4TAIL_BYTES },  /* %  37 */
   { (unsigned char) 50, (unsigned char)NO4TAIL_BYTES },  /* &  38 */
   { (unsigned char) 40, (unsigned char)NO4TAIL_BYTES },  /* '  39 */
   { (unsigned char) 51, (unsigned char)NO4TAIL_BYTES },  /* (  40 */
   { (unsigned char) 52, (unsigned char)NO4TAIL_BYTES },  /* )  41 */
   { (unsigned char) 53, (unsigned char)NO4TAIL_BYTES },  /* *  42 */
   { (unsigned char) 88, (unsigned char)NO4TAIL_BYTES },  /* +  43 */
   { (unsigned char) 54, (unsigned char)NO4TAIL_BYTES },  /* ,  44 */
   { (unsigned char) 41, (unsigned char)NO4TAIL_BYTES },  /* -  45 */
   { (unsigned char) 55, (unsigned char)NO4TAIL_BYTES },  /* .  46 */
   { (unsigned char) 56, (unsigned char)NO4TAIL_BYTES },  /* /  47 */
   { (unsigned char)112, (unsigned char)NO4TAIL_BYTES },  /* 0  48 */
   { (unsigned char)113, (unsigned char)NO4TAIL_BYTES },  /* 1  49 */
   { (unsigned char)114, (unsigned char)NO4TAIL_BYTES },  /* 2  50 */
   { (unsigned char)115, (unsigned char)NO4TAIL_BYTES },  /* 3  51 */
   { (unsigned char)116, (unsigned char)NO4TAIL_BYTES },  /* 4  52 */
   { (unsigned char)117, (unsigned char)NO4TAIL_BYTES },  /* 5  53 */
   { (unsigned char)118, (unsigned char)NO4TAIL_BYTES },  /* 6  54 */
   { (unsigned char)119, (unsigned char)NO4TAIL_BYTES },  /* 7  55 */
   { (unsigned char)120, (unsigned char)NO4TAIL_BYTES },  /* 8  56 */
   { (unsigned char)121, (unsigned char)NO4TAIL_BYTES },  /* 9  57 */
   { (unsigned char) 57, (unsigned char)NO4TAIL_BYTES },  /* :  58 */
   { (unsigned char) 58, (unsigned char)NO4TAIL_BYTES },  /* ;  59 */
   { (unsigned char) 89, (unsigned char)NO4TAIL_BYTES },  /* <  60 */
   { (unsigned char) 90, (unsigned char)NO4TAIL_BYTES },  /* =  61 */
   { (unsigned char) 91, (unsigned char)NO4TAIL_BYTES },  /* >  62 */
   { (unsigned char) 59, (unsigned char)NO4TAIL_BYTES },  /* ?  63 */
   { (unsigned char) 60, (unsigned char)NO4TAIL_BYTES },  /* @  64 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* A  65 */
   { (unsigned char)134, (unsigned char)NO4TAIL_BYTES },  /* B  66 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* C  67 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* D  68 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* E  69 */
   { (unsigned char)160, (unsigned char)NO4TAIL_BYTES },  /* F  70 */
   { (unsigned char)162, (unsigned char)NO4TAIL_BYTES },  /* G  71 */
   { (unsigned char)164, (unsigned char)NO4TAIL_BYTES },  /* H  72 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* I  73 */
   { (unsigned char)172, (unsigned char)NO4TAIL_BYTES },  /* J  74 */
   { (unsigned char)174, (unsigned char)NO4TAIL_BYTES },  /* K  75 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* L  76 */
   { (unsigned char)184, (unsigned char)NO4TAIL_BYTES },  /* M  77 */
   { (unsigned char)186, (unsigned char)NO4TAIL_BYTES },  /* N  78 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* O  79 */
   { (unsigned char)202, (unsigned char)NO4TAIL_BYTES },  /* P  80 */
   { (unsigned char)204, (unsigned char)NO4TAIL_BYTES },  /* Q  81 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* R  82 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* S  83 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* T  84 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* U  85 */
   { (unsigned char)238, (unsigned char)NO4TAIL_BYTES },  /* V  86 */
   { (unsigned char)240, (unsigned char)NO4TAIL_BYTES },  /* W  87 */
   { (unsigned char)242, (unsigned char)NO4TAIL_BYTES },  /* X  88 */
   { (unsigned char)244, (unsigned char)NO4TAIL_BYTES },  /* Y  89 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* Z  90 */
   { (unsigned char) 61, (unsigned char)NO4TAIL_BYTES },  /* [  91 */
   { (unsigned char) 62, (unsigned char)NO4TAIL_BYTES },  /* \  92 */
   { (unsigned char) 63, (unsigned char)NO4TAIL_BYTES },  /* ]  93 */
   { (unsigned char) 64, (unsigned char)NO4TAIL_BYTES },  /* ^  94 */
   { (unsigned char) 65, (unsigned char)NO4TAIL_BYTES },  /* _  95 */
   { (unsigned char) 66, (unsigned char)NO4TAIL_BYTES },  /* `  96 */
   { (unsigned char)123, (unsigned char)NO4TAIL_BYTES },  /* a  97 */
   { (unsigned char)135, (unsigned char)NO4TAIL_BYTES },  /* b  98 */
   { (unsigned char)137, (unsigned char)NO4TAIL_BYTES },  /* c  99 */
   { (unsigned char)145, (unsigned char)NO4TAIL_BYTES },  /* d 100 */
   { (unsigned char)151, (unsigned char)NO4TAIL_BYTES },  /* e 101 */
   { (unsigned char)161, (unsigned char)NO4TAIL_BYTES },  /* f 102 */
   { (unsigned char)163, (unsigned char)NO4TAIL_BYTES },  /* g 103 */
   { (unsigned char)165, (unsigned char)NO4TAIL_BYTES },  /* h 104 */
   { (unsigned char)167, (unsigned char)NO4TAIL_BYTES },  /* i 105 */
   { (unsigned char)173, (unsigned char)NO4TAIL_BYTES },  /* j 106 */
   { (unsigned char)175, (unsigned char)NO4TAIL_BYTES },  /* k 107 */
   { (unsigned char)177, (unsigned char)NO4TAIL_BYTES },  /* l 108 */
   { (unsigned char)185, (unsigned char)NO4TAIL_BYTES },  /* m 109 */
   { (unsigned char)187, (unsigned char)NO4TAIL_BYTES },  /* n 110 */
   { (unsigned char)193, (unsigned char)NO4TAIL_BYTES },  /* o 111 */
   { (unsigned char)203, (unsigned char)NO4TAIL_BYTES },  /* p 112 */
   { (unsigned char)205, (unsigned char)NO4TAIL_BYTES },  /* q 113 */
   { (unsigned char)207, (unsigned char)NO4TAIL_BYTES },  /* r 114 */
   { (unsigned char)213, (unsigned char)NO4TAIL_BYTES },  /* s 115 */
   { (unsigned char)222, (unsigned char)NO4TAIL_BYTES },  /* t 116 */
   { (unsigned char)229, (unsigned char)NO4TAIL_BYTES },  /* u 117 */
   { (unsigned char)239, (unsigned char)NO4TAIL_BYTES },  /* v 118 */
   { (unsigned char)241, (unsigned char)NO4TAIL_BYTES },  /* w 119 */
   { (unsigned char)243, (unsigned char)NO4TAIL_BYTES },  /* x 120 */
   { (unsigned char)245, (unsigned char)NO4TAIL_BYTES },  /* y 121 */
   { (unsigned char)249, (unsigned char)NO4TAIL_BYTES },  /* z 122 */
   { (unsigned char) 67, (unsigned char)NO4TAIL_BYTES },  /* { 123 */
   { (unsigned char) 68, (unsigned char)NO4TAIL_BYTES },  /* | 124 */
   { (unsigned char) 69, (unsigned char)NO4TAIL_BYTES },  /* } 125 */
   { (unsigned char) 70, (unsigned char)NO4TAIL_BYTES },  /*   126 */
   { (unsigned char) 34, (unsigned char)NO4TAIL_BYTES },  /* ≤ 127 */
   { (unsigned char)111, (unsigned char)NO4TAIL_BYTES },  /* Ä 128 */
   { (unsigned char) 35, (unsigned char)NO4TAIL_BYTES },  /* Å 129 */
   { (unsigned char) 82, (unsigned char)NO4TAIL_BYTES },  /* Ç 130 */
   { (unsigned char) 36, (unsigned char)NO4TAIL_BYTES },  /* É 131 */
   { (unsigned char) 85, (unsigned char)NO4TAIL_BYTES },  /* Ñ 132 */
   { (unsigned char)109, (unsigned char)NO4TAIL_BYTES },  /* Ö 133 */
   { (unsigned char)106, (unsigned char)NO4TAIL_BYTES },  /* Ü 134 */
   { (unsigned char)107, (unsigned char)NO4TAIL_BYTES },  /* á 135 */
   { (unsigned char) 37, (unsigned char)NO4TAIL_BYTES },  /* à 136 */
   { (unsigned char)110, (unsigned char)NO4TAIL_BYTES },  /* â 137 */
   { (unsigned char)219, (unsigned char)NO4TAIL_BYTES },  /* ä 138 */
   { (unsigned char) 86, (unsigned char)NO4TAIL_BYTES },  /* ã 139 */
   { (unsigned char)214, (unsigned char)NO4TAIL_BYTES },  /* å 140 */
   { (unsigned char)223, (unsigned char)NO4TAIL_BYTES },  /* ç 141 */
   { (unsigned char)254, (unsigned char)NO4TAIL_BYTES },  /* é 142 */
   { (unsigned char)250, (unsigned char)NO4TAIL_BYTES },  /* è 143 */
   { (unsigned char) 38, (unsigned char)NO4TAIL_BYTES },  /* ê 144 */
   { (unsigned char) 80, (unsigned char)NO4TAIL_BYTES },  /* ë 145 */
   { (unsigned char) 81, (unsigned char)NO4TAIL_BYTES },  /* í 146 */
   { (unsigned char) 83, (unsigned char)NO4TAIL_BYTES },  /* ì 147 */
   { (unsigned char) 84, (unsigned char)NO4TAIL_BYTES },  /* î 148 */
   { (unsigned char)108, (unsigned char)NO4TAIL_BYTES },  /* ï 149 */
   { (unsigned char) 43, (unsigned char)NO4TAIL_BYTES },  /* ñ 150 */
   { (unsigned char) 44, (unsigned char)NO4TAIL_BYTES },  /* ó 151 */
   { (unsigned char) 39, (unsigned char)NO4TAIL_BYTES },  /* ò 152 */
   { (unsigned char)227, (unsigned char)NO4TAIL_BYTES },  /* ô 153 */
   { (unsigned char)220, (unsigned char)NO4TAIL_BYTES },  /* ö 154 */
   { (unsigned char) 87, (unsigned char)NO4TAIL_BYTES },  /* ≤ 155 */
   { (unsigned char)215, (unsigned char)NO4TAIL_BYTES },  /* ≤ 156 */
   { (unsigned char)224, (unsigned char)NO4TAIL_BYTES },  /* ≤ 157 */
   { (unsigned char)255, (unsigned char)NO4TAIL_BYTES },  /* ≤ 158 */
   { (unsigned char)251, (unsigned char)NO4TAIL_BYTES },  /* ≤ 159 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* † 160 */
   { (unsigned char) 75, (unsigned char)NO4TAIL_BYTES },  /* ° 161 */
   { (unsigned char) 76, (unsigned char)NO4TAIL_BYTES },  /* ¢ 162 */
   { (unsigned char)182, (unsigned char)NO4TAIL_BYTES },  /* £ 163 */
   { (unsigned char) 97, (unsigned char)NO4TAIL_BYTES },  /* § 164 */
   { (unsigned char)132, (unsigned char)NO4TAIL_BYTES },  /* • 165 */
   { (unsigned char) 71, (unsigned char)NO4TAIL_BYTES },  /* ≤ 166 */
   { (unsigned char) 98, (unsigned char)NO4TAIL_BYTES },  /* ≤ 167 */
   { (unsigned char) 72, (unsigned char)NO4TAIL_BYTES },  /* ≤ 168 */
   { (unsigned char) 99, (unsigned char)NO4TAIL_BYTES },  /* ≤ 169 */
   { (unsigned char)216, (unsigned char)NO4TAIL_BYTES },  /* ≤ 170 */
   { (unsigned char) 93, (unsigned char)NO4TAIL_BYTES },  /* ≤ 171 */
   { (unsigned char)100, (unsigned char)NO4TAIL_BYTES },  /* ≤ 172 */
   { (unsigned char) 42, (unsigned char)NO4TAIL_BYTES },  /* ≤ 173 */
   { (unsigned char)101, (unsigned char)NO4TAIL_BYTES },  /* ≤ 174 */
   { (unsigned char)252, (unsigned char)NO4TAIL_BYTES },  /* ≤ 175 */
   { (unsigned char)102, (unsigned char)NO4TAIL_BYTES },  /* ≤ 176 */
   { (unsigned char) 92, (unsigned char)NO4TAIL_BYTES },  /* ≤ 177 */
   { (unsigned char) 78, (unsigned char)NO4TAIL_BYTES },  /* ≤ 178 */
   { (unsigned char)183, (unsigned char)NO4TAIL_BYTES },  /* ≤ 179 */
   { (unsigned char) 73, (unsigned char)NO4TAIL_BYTES },  /* ≤ 180 */
   { (unsigned char)103, (unsigned char)NO4TAIL_BYTES },  /* ≤ 181 */
   { (unsigned char)104, (unsigned char)NO4TAIL_BYTES },  /* ≤ 182 */
   { (unsigned char)105, (unsigned char)NO4TAIL_BYTES },  /* ≤ 183 */
   { (unsigned char) 74, (unsigned char)NO4TAIL_BYTES },  /* ≤ 184 */
   { (unsigned char)133, (unsigned char)NO4TAIL_BYTES },  /* ≤ 185 */
   { (unsigned char)217, (unsigned char)NO4TAIL_BYTES },  /* ≤ 186 */
   { (unsigned char) 94, (unsigned char)NO4TAIL_BYTES },  /* ≤ 187 */
   { (unsigned char)178, (unsigned char)NO4TAIL_BYTES },  /* ≤ 188 */
   { (unsigned char) 79, (unsigned char)NO4TAIL_BYTES },  /* ≤ 189 */
   { (unsigned char)179, (unsigned char)NO4TAIL_BYTES },  /* ≤ 190 */
   { (unsigned char)253, (unsigned char)NO4TAIL_BYTES },  /* ≤ 191 */
   { (unsigned char)210, (unsigned char)NO4TAIL_BYTES },  /* ≤ 192 */
   { (unsigned char)126, (unsigned char)NO4TAIL_BYTES },  /* ≤ 193 */
   { (unsigned char)128, (unsigned char)NO4TAIL_BYTES },  /* ≤ 194 */
   { (unsigned char)124, (unsigned char)NO4TAIL_BYTES },  /* ≤ 195 */
   { (unsigned char)130, (unsigned char)NO4TAIL_BYTES },  /* ≤ 196 */
   { (unsigned char)180, (unsigned char)NO4TAIL_BYTES },  /* ≤ 197 */
   { (unsigned char)142, (unsigned char)NO4TAIL_BYTES },  /* ≤ 198 */
   { (unsigned char)138, (unsigned char)NO4TAIL_BYTES },  /* ≤ 199 */
   { (unsigned char)140, (unsigned char)NO4TAIL_BYTES },  /* ≤ 200 */
   { (unsigned char)154, (unsigned char)NO4TAIL_BYTES },  /* ≤ 201 */
   { (unsigned char)158, (unsigned char)NO4TAIL_BYTES },  /* ≤ 202 */
   { (unsigned char)156, (unsigned char)NO4TAIL_BYTES },  /* ≤ 203 */
   { (unsigned char)152, (unsigned char)NO4TAIL_BYTES },  /* ≤ 204 */
   { (unsigned char)168, (unsigned char)NO4TAIL_BYTES },  /* ≤ 205 */
   { (unsigned char)170, (unsigned char)NO4TAIL_BYTES },  /* ≤ 206 */
   { (unsigned char)146, (unsigned char)NO4TAIL_BYTES },  /* ≤ 207 */
   { (unsigned char)148, (unsigned char)NO4TAIL_BYTES },  /* ≤ 208 */
   { (unsigned char)190, (unsigned char)NO4TAIL_BYTES },  /* ≤ 209 */
   { (unsigned char)188, (unsigned char)NO4TAIL_BYTES },  /* ≤ 210 */
   { (unsigned char)194, (unsigned char)NO4TAIL_BYTES },  /* ≤ 211 */
   { (unsigned char)196, (unsigned char)NO4TAIL_BYTES },  /* ≤ 212 */
   { (unsigned char)200, (unsigned char)NO4TAIL_BYTES },  /* ≤ 213 */
   { (unsigned char)198, (unsigned char)NO4TAIL_BYTES },  /* ≤ 214 */
   { (unsigned char) 95, (unsigned char)NO4TAIL_BYTES },  /* ≤ 215 */
   { (unsigned char)208, (unsigned char)NO4TAIL_BYTES },  /* ≤ 216 */
   { (unsigned char)234, (unsigned char)NO4TAIL_BYTES },  /* ≤ 217 */
   { (unsigned char)230, (unsigned char)NO4TAIL_BYTES },  /* ≤ 218 */
   { (unsigned char)236, (unsigned char)NO4TAIL_BYTES },  /* ≤ 219 */
   { (unsigned char)232, (unsigned char)NO4TAIL_BYTES },  /* ≤ 220 */
   { (unsigned char)246, (unsigned char)NO4TAIL_BYTES },  /* ≤ 221 */
   { (unsigned char)225, (unsigned char)NO4TAIL_BYTES },  /* ≤ 222 */
   { (unsigned char)218, (unsigned char)NO4TAIL_BYTES },  /* ≤ 223 */
   { (unsigned char)211, (unsigned char)NO4TAIL_BYTES },  /* ≤ 224 */
   { (unsigned char)127, (unsigned char)NO4TAIL_BYTES },  /* · 225*/
   { (unsigned char)129, (unsigned char)NO4TAIL_BYTES },  /* ≤ 226 */
   { (unsigned char)125, (unsigned char)NO4TAIL_BYTES },  /* ≤ 227 */
   { (unsigned char)131, (unsigned char)NO4TAIL_BYTES },  /* ≤ 228 */
   { (unsigned char)181, (unsigned char)NO4TAIL_BYTES },  /* ≤ 229 */
   { (unsigned char)143, (unsigned char)NO4TAIL_BYTES },  /* ≤ 230 */
   { (unsigned char)139, (unsigned char)NO4TAIL_BYTES },  /* ≤ 231 */
   { (unsigned char)141, (unsigned char)NO4TAIL_BYTES },  /* ≤ 232 */
   { (unsigned char)155, (unsigned char)NO4TAIL_BYTES },  /* ≤ 233 */
   { (unsigned char)159, (unsigned char)NO4TAIL_BYTES },  /* ≤ 234 */
   { (unsigned char)157, (unsigned char)NO4TAIL_BYTES },  /* ≤ 235 */
   { (unsigned char)153, (unsigned char)NO4TAIL_BYTES },  /* ≤ 236 */
   { (unsigned char)169, (unsigned char)NO4TAIL_BYTES },  /* ≤ 237 */
   { (unsigned char)171, (unsigned char)NO4TAIL_BYTES },  /* ≤ 238 */
   { (unsigned char)147, (unsigned char)NO4TAIL_BYTES },  /* ≤ 239 */
   { (unsigned char)149, (unsigned char)NO4TAIL_BYTES },  /* ≤ 240 */
   { (unsigned char)191, (unsigned char)NO4TAIL_BYTES },  /* ≤ 241 */
   { (unsigned char)189, (unsigned char)NO4TAIL_BYTES },  /* ≤ 242 */
   { (unsigned char)195, (unsigned char)NO4TAIL_BYTES },  /* ≤ 243 */
   { (unsigned char)197, (unsigned char)NO4TAIL_BYTES },  /* ≤ 244 */
   { (unsigned char)201, (unsigned char)NO4TAIL_BYTES },  /* ≤ 245 */
   { (unsigned char)199, (unsigned char)NO4TAIL_BYTES },  /* ≤ 246 */
   { (unsigned char) 96, (unsigned char)NO4TAIL_BYTES },  /* ≤ 247 */
   { (unsigned char)209, (unsigned char)NO4TAIL_BYTES },  /* ≤ 248 */
   { (unsigned char)235, (unsigned char)NO4TAIL_BYTES },  /* ≤ 249 */
   { (unsigned char)231, (unsigned char)NO4TAIL_BYTES },  /* ≤ 250 */
   { (unsigned char)237, (unsigned char)NO4TAIL_BYTES },  /* ≤ 251 */
   { (unsigned char)233, (unsigned char)NO4TAIL_BYTES },  /* ≤ 252 */
   { (unsigned char)247, (unsigned char)NO4TAIL_BYTES },  /* ≤ 253 */
   { (unsigned char)226, (unsigned char)NO4TAIL_BYTES },  /* ≤ 254 */
   { (unsigned char) 77, (unsigned char)NO4TAIL_BYTES },  /* ≤ 255 */
} ;



// AS Jan 8/03 - Support for CodePage 1250, croation sorting case insensitive
static Translate4arrayChar cp1250croatianUpperCollationArray[] =
{
   /* head, tail */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   0 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   1 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   2 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   3 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   4 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   5 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   6 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   7 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   8 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤   9 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  10 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  11 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  12 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  13 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  14 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* ≤  15 */
   { (unsigned char) 18, (unsigned char)NO4TAIL_BYTES },  /* ≤  16 */
   { (unsigned char) 19, (unsigned char)NO4TAIL_BYTES },  /* ≤  17 */
   { (unsigned char) 20, (unsigned char)NO4TAIL_BYTES },  /* ≤  18 */
   { (unsigned char) 21, (unsigned char)NO4TAIL_BYTES },  /* ≤  19 */
   { (unsigned char) 22, (unsigned char)NO4TAIL_BYTES },  /* ≤  20 */
   { (unsigned char) 23, (unsigned char)NO4TAIL_BYTES },  /* ≤  21 */
   { (unsigned char) 24, (unsigned char)NO4TAIL_BYTES },  /* ≤  22 */
   { (unsigned char) 25, (unsigned char)NO4TAIL_BYTES },  /* ≤  23 */
   { (unsigned char) 26, (unsigned char)NO4TAIL_BYTES },  /* ≤  24 */
   { (unsigned char) 27, (unsigned char)NO4TAIL_BYTES },  /* ≤  25 */
   { (unsigned char) 28, (unsigned char)NO4TAIL_BYTES },  /* ≤  26 */
   { (unsigned char) 29, (unsigned char)NO4TAIL_BYTES },  /* ≤  27 */
   { (unsigned char) 30, (unsigned char)NO4TAIL_BYTES },  /* ≤  28 */
   { (unsigned char) 31, (unsigned char)NO4TAIL_BYTES },  /* ≤  29 */
   { (unsigned char) 32, (unsigned char)NO4TAIL_BYTES },  /* ≤  30 */
   { (unsigned char) 33, (unsigned char)NO4TAIL_BYTES },  /* ≤  31 */
   { (unsigned char) 17, (unsigned char)NO4TAIL_BYTES },  /*    32 */
   { (unsigned char) 45, (unsigned char)NO4TAIL_BYTES },  /* !  33 */
   { (unsigned char) 46, (unsigned char)NO4TAIL_BYTES },  /* "  34 */
   { (unsigned char) 47, (unsigned char)NO4TAIL_BYTES },  /* #  35 */
   { (unsigned char) 48, (unsigned char)NO4TAIL_BYTES },  /* $  36 */
   { (unsigned char) 49, (unsigned char)NO4TAIL_BYTES },  /* %  37 */
   { (unsigned char) 50, (unsigned char)NO4TAIL_BYTES },  /* &  38 */
   { (unsigned char) 40, (unsigned char)NO4TAIL_BYTES },  /* '  39 */
   { (unsigned char) 51, (unsigned char)NO4TAIL_BYTES },  /* (  40 */
   { (unsigned char) 52, (unsigned char)NO4TAIL_BYTES },  /* )  41 */
   { (unsigned char) 53, (unsigned char)NO4TAIL_BYTES },  /* *  42 */
   { (unsigned char) 88, (unsigned char)NO4TAIL_BYTES },  /* +  43 */
   { (unsigned char) 54, (unsigned char)NO4TAIL_BYTES },  /* ,  44 */
   { (unsigned char) 41, (unsigned char)NO4TAIL_BYTES },  /* -  45 */
   { (unsigned char) 55, (unsigned char)NO4TAIL_BYTES },  /* .  46 */
   { (unsigned char) 56, (unsigned char)NO4TAIL_BYTES },  /* /  47 */
   { (unsigned char)112, (unsigned char)NO4TAIL_BYTES },  /* 0  48 */
   { (unsigned char)113, (unsigned char)NO4TAIL_BYTES },  /* 1  49 */
   { (unsigned char)114, (unsigned char)NO4TAIL_BYTES },  /* 2  50 */
   { (unsigned char)115, (unsigned char)NO4TAIL_BYTES },  /* 3  51 */
   { (unsigned char)116, (unsigned char)NO4TAIL_BYTES },  /* 4  52 */
   { (unsigned char)117, (unsigned char)NO4TAIL_BYTES },  /* 5  53 */
   { (unsigned char)118, (unsigned char)NO4TAIL_BYTES },  /* 6  54 */
   { (unsigned char)119, (unsigned char)NO4TAIL_BYTES },  /* 7  55 */
   { (unsigned char)120, (unsigned char)NO4TAIL_BYTES },  /* 8  56 */
   { (unsigned char)121, (unsigned char)NO4TAIL_BYTES },  /* 9  57 */
   { (unsigned char) 57, (unsigned char)NO4TAIL_BYTES },  /* :  58 */
   { (unsigned char) 58, (unsigned char)NO4TAIL_BYTES },  /* ;  59 */
   { (unsigned char) 89, (unsigned char)NO4TAIL_BYTES },  /* <  60 */
   { (unsigned char) 90, (unsigned char)NO4TAIL_BYTES },  /* =  61 */
   { (unsigned char) 91, (unsigned char)NO4TAIL_BYTES },  /* >  62 */
   { (unsigned char) 59, (unsigned char)NO4TAIL_BYTES },  /* ?  63 */
   { (unsigned char) 60, (unsigned char)NO4TAIL_BYTES },  /* @  64 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* A  65 */
   { (unsigned char)134, (unsigned char)NO4TAIL_BYTES },  /* B  66 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* C  67 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* D  68 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* E  69 */
   { (unsigned char)160, (unsigned char)NO4TAIL_BYTES },  /* F  70 */
   { (unsigned char)162, (unsigned char)NO4TAIL_BYTES },  /* G  71 */
   { (unsigned char)164, (unsigned char)NO4TAIL_BYTES },  /* H  72 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* I  73 */
   { (unsigned char)172, (unsigned char)NO4TAIL_BYTES },  /* J  74 */
   { (unsigned char)174, (unsigned char)NO4TAIL_BYTES },  /* K  75 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* L  76 */
   { (unsigned char)184, (unsigned char)NO4TAIL_BYTES },  /* M  77 */
   { (unsigned char)186, (unsigned char)NO4TAIL_BYTES },  /* N  78 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* O  79 */
   { (unsigned char)202, (unsigned char)NO4TAIL_BYTES },  /* P  80 */
   { (unsigned char)204, (unsigned char)NO4TAIL_BYTES },  /* Q  81 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* R  82 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* S  83 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* T  84 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* U  85 */
   { (unsigned char)238, (unsigned char)NO4TAIL_BYTES },  /* V  86 */
   { (unsigned char)240, (unsigned char)NO4TAIL_BYTES },  /* W  87 */
   { (unsigned char)242, (unsigned char)NO4TAIL_BYTES },  /* X  88 */
   { (unsigned char)244, (unsigned char)NO4TAIL_BYTES },  /* Y  89 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* Z  90 */
   { (unsigned char) 61, (unsigned char)NO4TAIL_BYTES },  /* [  91 */
   { (unsigned char) 62, (unsigned char)NO4TAIL_BYTES },  /* \  92 */
   { (unsigned char) 63, (unsigned char)NO4TAIL_BYTES },  /* ]  93 */
   { (unsigned char) 64, (unsigned char)NO4TAIL_BYTES },  /* ^  94 */
   { (unsigned char) 65, (unsigned char)NO4TAIL_BYTES },  /* _  95 */
   { (unsigned char) 66, (unsigned char)NO4TAIL_BYTES },  /* `  96 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* a  97 */
   { (unsigned char)134, (unsigned char)NO4TAIL_BYTES },  /* b  98 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* c  99 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* d 100 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* e 101 */
   { (unsigned char)160, (unsigned char)NO4TAIL_BYTES },  /* f 102 */
   { (unsigned char)162, (unsigned char)NO4TAIL_BYTES },  /* g 103 */
   { (unsigned char)164, (unsigned char)NO4TAIL_BYTES },  /* h 104 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* i 105 */
   { (unsigned char)172, (unsigned char)NO4TAIL_BYTES },  /* j 106 */
   { (unsigned char)174, (unsigned char)NO4TAIL_BYTES },  /* k 107 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* l 108 */
   { (unsigned char)184, (unsigned char)NO4TAIL_BYTES },  /* m 109 */
   { (unsigned char)186, (unsigned char)NO4TAIL_BYTES },  /* n 110 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* o 111 */
   { (unsigned char)202, (unsigned char)NO4TAIL_BYTES },  /* p 112 */
   { (unsigned char)204, (unsigned char)NO4TAIL_BYTES },  /* q 113 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* r 114 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* s 115 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* t 116 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* u 117 */
   { (unsigned char)238, (unsigned char)NO4TAIL_BYTES },  /* v 118 */
   { (unsigned char)240, (unsigned char)NO4TAIL_BYTES },  /* w 119 */
   { (unsigned char)242, (unsigned char)NO4TAIL_BYTES },  /* x 120 */
   { (unsigned char)244, (unsigned char)NO4TAIL_BYTES },  /* y 121 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* z 122 */
   { (unsigned char) 67, (unsigned char)NO4TAIL_BYTES },  /* { 123 */
   { (unsigned char) 68, (unsigned char)NO4TAIL_BYTES },  /* | 124 */
   { (unsigned char) 69, (unsigned char)NO4TAIL_BYTES },  /* } 125 */
   { (unsigned char) 70, (unsigned char)NO4TAIL_BYTES },  /*   126 */
   { (unsigned char) 34, (unsigned char)NO4TAIL_BYTES },  /* ≤ 127 */
   { (unsigned char)111, (unsigned char)NO4TAIL_BYTES },  /* Ä 128 */
   { (unsigned char) 35, (unsigned char)NO4TAIL_BYTES },  /* Å 129 */
   { (unsigned char) 82, (unsigned char)NO4TAIL_BYTES },  /* Ç 130 */
   { (unsigned char) 36, (unsigned char)NO4TAIL_BYTES },  /* É 131 */
   { (unsigned char) 85, (unsigned char)NO4TAIL_BYTES },  /* Ñ 132 */
   { (unsigned char)109, (unsigned char)NO4TAIL_BYTES },  /* Ö 133 */
   { (unsigned char)106, (unsigned char)NO4TAIL_BYTES },  /* Ü 134 */
   { (unsigned char)107, (unsigned char)NO4TAIL_BYTES },  /* á 135 */
   { (unsigned char) 37, (unsigned char)NO4TAIL_BYTES },  /* à 136 */
   { (unsigned char)110, (unsigned char)NO4TAIL_BYTES },  /* â 137 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* ä 138 */
   { (unsigned char) 86, (unsigned char)NO4TAIL_BYTES },  /* ã 139 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* å 140 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* ç 141 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* é 142 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* è 143 */
   { (unsigned char) 38, (unsigned char)NO4TAIL_BYTES },  /* ê 144 */
   { (unsigned char) 80, (unsigned char)NO4TAIL_BYTES },  /* ë 145 */
   { (unsigned char) 81, (unsigned char)NO4TAIL_BYTES },  /* í 146 */
   { (unsigned char) 83, (unsigned char)NO4TAIL_BYTES },  /* ì 147 */
   { (unsigned char) 84, (unsigned char)NO4TAIL_BYTES },  /* î 148 */
   { (unsigned char)108, (unsigned char)NO4TAIL_BYTES },  /* ï 149 */
   { (unsigned char) 43, (unsigned char)NO4TAIL_BYTES },  /* ñ 150 */
   { (unsigned char) 44, (unsigned char)NO4TAIL_BYTES },  /* ó 151 */
   { (unsigned char) 39, (unsigned char)NO4TAIL_BYTES },  /* ò 152 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* ô 153 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* ö 154 */
   { (unsigned char) 87, (unsigned char)NO4TAIL_BYTES },  /* ≤ 155 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* ≤ 156 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* ≤ 157 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* ≤ 158 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* ≤ 159 */
   { (unsigned char) 16, (unsigned char)NO4TAIL_BYTES },  /* † 160 */
   { (unsigned char) 75, (unsigned char)NO4TAIL_BYTES },  /* ° 161 */
   { (unsigned char) 76, (unsigned char)NO4TAIL_BYTES },  /* ¢ 162 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* £ 163 */
   { (unsigned char) 97, (unsigned char)NO4TAIL_BYTES },  /* § 164 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* • 165 */
   { (unsigned char) 71, (unsigned char)NO4TAIL_BYTES },  /* ≤ 166 */
   { (unsigned char) 98, (unsigned char)NO4TAIL_BYTES },  /* ≤ 167 */
   { (unsigned char) 72, (unsigned char)NO4TAIL_BYTES },  /* ≤ 168 */
   { (unsigned char) 99, (unsigned char)NO4TAIL_BYTES },  /* ≤ 169 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* ≤ 170 */
   { (unsigned char) 93, (unsigned char)NO4TAIL_BYTES },  /* ≤ 171 */
   { (unsigned char)100, (unsigned char)NO4TAIL_BYTES },  /* ≤ 172 */
   { (unsigned char) 42, (unsigned char)NO4TAIL_BYTES },  /* ≤ 173 */
   { (unsigned char)101, (unsigned char)NO4TAIL_BYTES },  /* ≤ 174 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* ≤ 175 */
   { (unsigned char)102, (unsigned char)NO4TAIL_BYTES },  /* ≤ 176 */
   { (unsigned char) 92, (unsigned char)NO4TAIL_BYTES },  /* ≤ 177 */
   { (unsigned char) 78, (unsigned char)NO4TAIL_BYTES },  /* ≤ 178 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* ≤ 179 */
   { (unsigned char) 73, (unsigned char)NO4TAIL_BYTES },  /* ≤ 180 */
   { (unsigned char)103, (unsigned char)NO4TAIL_BYTES },  /* ≤ 181 */
   { (unsigned char)104, (unsigned char)NO4TAIL_BYTES },  /* ≤ 182 */
   { (unsigned char)105, (unsigned char)NO4TAIL_BYTES },  /* ≤ 183 */
   { (unsigned char) 74, (unsigned char)NO4TAIL_BYTES },  /* ≤ 184 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 185 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* ≤ 186 */
   { (unsigned char) 94, (unsigned char)NO4TAIL_BYTES },  /* ≤ 187 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* ≤ 188 */
   { (unsigned char) 79, (unsigned char)NO4TAIL_BYTES },  /* ≤ 189 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* ≤ 190 */
   { (unsigned char)248, (unsigned char)NO4TAIL_BYTES },  /* ≤ 191 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* ≤ 192 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 193 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 194 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 195 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 196 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* ≤ 197 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* ≤ 198 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* ≤ 199 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* ≤ 200 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 201 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 202 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 203 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 204 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* ≤ 205 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* ≤ 206 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* ≤ 207 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* ≤ 208 */
   { (unsigned char)188, (unsigned char)NO4TAIL_BYTES },  /* ≤ 209 */
   { (unsigned char)188, (unsigned char)NO4TAIL_BYTES },  /* ≤ 210 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 211 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 212 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 213 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 214 */
   { (unsigned char) 95, (unsigned char)NO4TAIL_BYTES },  /* ≤ 215 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* ≤ 216 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 217 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 218 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 219 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 220 */
   { (unsigned char)244, (unsigned char)NO4TAIL_BYTES },  /* ≤ 221 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* ≤ 222 */
   { (unsigned char)212, (unsigned char)NO4TAIL_BYTES },  /* ≤ 223 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* ≤ 224 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* · 225*/
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 226 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 227 */
   { (unsigned char)122, (unsigned char)NO4TAIL_BYTES },  /* ≤ 228 */
   { (unsigned char)176, (unsigned char)NO4TAIL_BYTES },  /* ≤ 229 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* ≤ 230 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* ≤ 231 */
   { (unsigned char)136, (unsigned char)NO4TAIL_BYTES },  /* ≤ 232 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 233 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 234 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 235 */
   { (unsigned char)150, (unsigned char)NO4TAIL_BYTES },  /* ≤ 236 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* ≤ 237 */
   { (unsigned char)166, (unsigned char)NO4TAIL_BYTES },  /* ≤ 238 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* ≤ 239 */
   { (unsigned char)144, (unsigned char)NO4TAIL_BYTES },  /* ≤ 240 */
   { (unsigned char)188, (unsigned char)NO4TAIL_BYTES },  /* ≤ 241 */
   { (unsigned char)188, (unsigned char)NO4TAIL_BYTES },  /* ≤ 242 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 243 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 244 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 245 */
   { (unsigned char)192, (unsigned char)NO4TAIL_BYTES },  /* ≤ 246 */
   { (unsigned char) 96, (unsigned char)NO4TAIL_BYTES },  /* ≤ 247 */
   { (unsigned char)206, (unsigned char)NO4TAIL_BYTES },  /* ≤ 248 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 249 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 250 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 251 */
   { (unsigned char)228, (unsigned char)NO4TAIL_BYTES },  /* ≤ 252 */
   { (unsigned char)244, (unsigned char)NO4TAIL_BYTES },  /* ≤ 253 */
   { (unsigned char)221, (unsigned char)NO4TAIL_BYTES },  /* ≤ 254 */
   { (unsigned char) 77, (unsigned char)NO4TAIL_BYTES },  /* ≤ 255 */
} ;
