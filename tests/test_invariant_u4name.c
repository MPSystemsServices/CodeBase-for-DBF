#include <check.h>
#include <stdlib.h>
#include <string.h>

/* Declare the function from u4name.c - typically exposed via header */
extern int u4nameCurrent(char *outName, int outNameLen, const char *inputName);

#define OUTBUF_SIZE 256

START_TEST(test_buffer_overflow_protection)
{
    /* Invariant: Buffer reads/writes never exceed declared outName length */
    const char *payloads[] = {
        /* Exact exploit: 2x buffer size */
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        /* Boundary: exactly at buffer limit */
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
        "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
        /* Valid input: normal filename */
        "testfile.dat"
    };
    int num_payloads = sizeof(payloads) / sizeof(payloads[0]);

    for (int i = 0; i < num_payloads; i++) {
        char outName[OUTBUF_SIZE];
        char canary[16];
        
        memset(outName, 0, OUTBUF_SIZE);
        memset(canary, 0xAB, sizeof(canary));
        
        /* Call the actual function - if it exists and is accessible */
        int result = u4nameCurrent(outName, OUTBUF_SIZE, payloads[i]);
        
        /* Invariant: output must be null-terminated within bounds */
        ck_assert_msg(outName[OUTBUF_SIZE - 1] == '\0' || strlen(outName) < OUTBUF_SIZE,
                      "Buffer overflow: output exceeds declared length for payload %d", i);
        
        /* Canary check - memory after buffer should be untouched */
        for (int j = 0; j < 16; j++) {
            ck_assert_msg(canary[j] == (char)0xAB,
                          "Stack corruption detected for payload %d", i);
        }
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_buffer_overflow_protection);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}