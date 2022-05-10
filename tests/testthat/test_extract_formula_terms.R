
frm_test <- y ~ x1 + x2+ x3 + x4
str(frm_test)
all.vars(frm_test)
terms(frm_test)
attr(terms(frm_test), "variables")
attr(terms(frm_test), "term.labels")
attr(terms(frm_test), "response")

frm_test <- . ~ x1 + x2+ x3 + x4
str(frm_test)
all.vars(frm_test)
terms(frm_test)
attr(terms(frm_test), "variables")
attr(terms(frm_test), "term.labels")
attr(terms(frm_test), "response")


frm_test <-  ~ x1 + x2+ x3 + x4
str(frm_test)
all.vars(frm_test)
terms(frm_test)
attr(terms(frm_test), "variables")
attr(terms(frm_test), "term.labels")
attr(terms(frm_test), "response")
