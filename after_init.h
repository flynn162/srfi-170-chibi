/* (define-c (free maybe-null string) pa-strerror (int)) */
op = sexp_define_foreign(ctx, env, "pa-strerror", 1, pa_strerror_stub);
