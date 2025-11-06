#ifndef RUST_UTILS_H
#define RUST_UTILS_H

#include "d4All.h"

#ifdef __cplusplus
extern "C" {
#endif

S4EXPORT int code4_initw(CODE4 *cb) {
    return code4init(cb);
}

S4EXPORT const char *t4filterw( TAG4 *tag ) {
    return t4filter(tag);
}

S4EXPORT const char *t4exprw( TAG4 *tag ) {
    return t4expr(tag);
}

#ifdef __cplusplus  
}
#endif

#endif /* RUST_UTILS_H */