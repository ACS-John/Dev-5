! Replace Test\ComboF
autoLibrary
fnTop(program$)
fnTos
! fncombof("fs-bal2",1,2,f1Col4Len,"[Q]\GLmstr\acglfnsc.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx1.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",0)
mypos=1
fnlbl(1,1,'-----------------------------------------------------------------------')
fncombof("payee",2,mypos,35,'[Q]\GLmstr\PayMstr.h[cno]',1,8,9,30,'[Q]\GLmstr\PayIdx1.h[cno]',0,0, 'If the payee number is known, the general ledger information can be extracted from that record.',0)
dim response$(5)*80
response$(1)='one' ! str$(10)
fnCmdSet(2)
fnAcs(mat response$,ckey)
pr mat response$
