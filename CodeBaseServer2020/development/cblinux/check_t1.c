#include <d4all.h>

void checkTag(DATA4 *data, const char *tagName, int rec1, int rec2, int rec3, int rec4, int rec5, int rec6, int rec7, int rec8)
{
   int rc;
   int expectedRec[9] = { rec1, rec2, rec3, rec4, rec5, rec6, rec7, rec8, 0};
   int i;

   d4tagSelect(data, d4tag(data, tagName));

   for (rc = d4top(data), i = 0;  ; rc = d4skip(data, 1L), i++)
   {
      if (expectedRec[i] == 0 && d4eof(data))
      {
         printf("Tag %s is good.\n", t4alias(d4tagSelected(data)));
         break;
      }

      if (d4eof(data))
      {
         printf("Tag %s is bad. Expected rec %d; instead got EOF. (%d)\n", t4alias(d4tagSelected(data)), expectedRec[i], i);
         break;
      }

      if (expectedRec[i] != d4recNo(data))
      {
         printf("Tag %s is bad. Expected rec %d; instead got rec %d. (%d)\n", t4alias(d4tagSelected(data)), expectedRec[i], d4recNo(data), i);
         break;
      }
   }
}

int main(int argc, char *argv[])
{
   CODE4 cb;
   DATA4 *data;
   FIELD4 *fbin1, *fbin2, *fchar1, *fchar2, *fmemobin1, *fmemobin2, *fmemo1, *fmemo2;
//   int i, fieldi;

   code4init(&cb);
   data = d4open(&cb, argv[1]);
   if (data)
   {
      fbin1 = d4field(data, "bin1");
      fbin2 = d4field(data, "bin2");
      fchar1 = d4field(data, "char1");
      fchar2 = d4field(data, "char2");
      fmemobin1 = d4field(data, "memobin1");
      fmemobin2 = d4field(data, "memobin2");
      fmemo1 = d4field(data, "memo1");
      fmemo2 = d4field(data, "memo2");

      if (d4check(data) != r4success)
      {
         fprintf(stderr, "Index is bad.");
      }
      else
      {
         checkTag(data, "bin1a",  7, 8, 6, 2, 1, 3, 4, 5);
         checkTag(data, "bin1b",  7, 1, 3, 5, 0, 0, 0, 0);
         checkTag(data, "bin1c",  7, 8, 6, 2, 1, 3, 4, 5);
         checkTag(data, "bin1d",  7, 1, 3, 5, 0, 0, 0, 0);
         checkTag(data, "bin2a",  7, 8, 2, 5, 4, 3, 6, 1);
         checkTag(data, "bin2b",  7, 5, 3, 1, 0, 0, 0, 0);
         checkTag(data, "bin2c",  7, 8, 2, 5, 4, 3, 6, 1);
         checkTag(data, "bin2d",  7, 5, 3, 1, 0, 0, 0, 0);
         checkTag(data, "char1a", 7, 8, 6, 2, 5, 3, 4, 1);
         checkTag(data, "char1b", 7, 5, 3, 1, 0, 0, 0, 0);
         checkTag(data, "char1c", 7, 8, 6, 2, 5, 3, 4, 1);
         checkTag(data, "char1d", 7, 5, 3, 1, 0, 0, 0, 0);
         checkTag(data, "char2a", 7, 8, 2, 3, 5, 4, 1, 6);
         checkTag(data, "char2b", 7, 3, 5, 1, 0, 0, 0, 0);
         checkTag(data, "char2c", 7, 8, 2, 3, 5, 4, 1, 6);
         checkTag(data, "char2d", 7, 3, 5, 1, 0, 0, 0, 0);
      }
   }

   code4initUndo(&cb);
   return 0;
}
