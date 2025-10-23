#ifndef RUST_UTILS_H
#define RUST_UTILS_H

#include "d4All.h"

#ifdef __cplusplus
extern "C" {
#endif

int code4_initw(CODE4 *cb) {
    return code4init(cb);
}

const char *t4filterw( TAG4 *tag ) {
    return t4filter(tag);
}

const char *t4exprw( TAG4 *tag ) {
    return t4expr(tag);
}

#ifdef __cplusplus  
}
#endif

#endif /* RUST_UTILS_H */