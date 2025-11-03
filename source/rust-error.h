#ifndef RUST_ERROR_H
#define RUST_ERROR_H

#include "d4all.h"

#ifdef __cplusplus
extern "C" {
#endif

extern void error_hook(CODE4 *cb, int code1, long code2, const char *message);

#ifdef __cplusplus
}
#endif

#endif /* RUST_ERROR_H */