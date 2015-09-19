
# non-ASCI tests moved here due to failing R-devel around 18 Sep 2015.
# Possibly due to these changes in R-devel on that date:
# * match(x, table) is faster, sometimes by an order of magnitude, when ‘x’ is of length one, thanks to Haverty's PR#16491.
# * abbreviate() has more support for multi-byte character sets
# Only tests 1092 and 1093 failed, but we've moved the others too as a belt-and-braces approach.
# Separating into its own package keeps data.table clean, robust and as stable as possible. Whilst still supporting and testing non-ASCII via this package.
# We can also change the Encoding field in DESCRIPTION in this package if helps, without having to affect the rest of data.table's tests.

# Bug fix for #5159 - chmatch and character encoding (for some reason this seems to pass the test on a mac as well)
a<-c("a","\u00E4","\u00DF","z")
au<-iconv(a,"UTF8","latin1")
expect_equal(label="1164.1", chmatch(a, au), match(a, au))

# chmatch on 'unknown' encoding (e.g. as.character(as.symbol("\u00E4")) )falling back to match, #2538 and #4818
x1 <- c("al\u00E4", "ala", "\u00E4allc", "coep")
x2 <- c("ala", "al\u00E4")
test(1088.1, chmatch(x1, x2), match(x1, x2)) # should not fallback to "match"
test(1088.2, x1 %chin% x2, x1 %in% x2)
# change x1 to symbol to character
x3 <- unlist(lapply(x1, function(x) as.character(as.name(x))), use.names=FALSE)
test(1089.1, chmatch(x3, x2), match(x3, x2)) # should fallback to match in "x"
test(1089.2, x3 %chin% x2, x3 %in% x2) # should fallback to match in "x"
# change x2 to symbol to character
x4 <- unlist(lapply(x2, function(x) as.character(as.name(x))), use.names=FALSE)
test(1090.1, chmatch(x1,x4), match(x1, x4)) # should fallback to match in "table"
test(1090.2, x1 %chin% x4, x1 %in% x4)
# both are symbols to characters
test(1091.1, chmatch(x3, x4), match(x3, x4)) # should fallback to "match" in "x" as well.
test(1091.2, x3 %chin% x4, x3 %in% x4)
# for completness, include test from #2528 of non ascii LHS of := (it could feasibly fail in future due to something other than chmatch)

# DT = data.table(pas = c(1:5, NA, 6:10), good = c(1:10, NA))
setnames(DT, "pas", "p\u00E4s")
test(1092, eval(parse(text="DT[is.na(p\u00E4s), p\u00E4s := 99L]")), data.table("p\u00E4s" = c(1:5, 99L, 6:10), good = c(1:10,NA)))
test(1093, eval(parse(text="DT[, p\u00E4s := 34L]")), data.table("p\u00E4s" = 34L, good=c(1:10,NA)))

# savetl_init error after error, in v1.9.2, thanks Arun
DT <- data.table(x=1:5, y=10:6)
test(1229.1, DT[forderv(DT, -1)], error="out of range.*1,2")
test(1229.2, setkey(DT), data.table(x=1:5, y=10:6, key="x,y"))
# umlaut in column names (red herring I think, but testing anyway)
sentEx = data.table(abend = c(1, 1, 0, 0, 2),
                    aber = c(0, 1, 0, 0, 0), 
                    "\u00FCber" = c(1, 0, 0, 0, 0),
                    "\u00FCberall" = c(0, 0, 0, 0, 0),
                    "\u00FCberlegt" = c(0, 0, 0, 0, 0),
                    ID = structure(c(1L, 1L, 2L, 2L, 2L), .Label = c("0019", "0021"), class = "factor"),
                    abgeandert = c(1, 1, 1, 0, 0),
                    abgebildet = c(0, 0, 1, 1, 0),
                    abgelegt = c(0, 0, 0, 0, 3))
test(1229.3, sentEx[, lapply(.SD, sum), by=ID], data.table(ID=factor(c("0019","0021")), abend=c(2,2), aber=c(1,0), "\u00FCber"=c(1,0),
             "\u00FCberall"=c(0,0), "\u00FCberlegt" = c(0,0), abgeandert=c(2,1), abgebildet = c(0,2), abgelegt=c(0,3)))


