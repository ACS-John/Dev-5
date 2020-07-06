! Replace Test\ComboF
autoLibrary
fnTop(program$,"Test Combobox from File")
fnTos
fncombof("fs-bal2",1,2,f1Col4Len,"[Q]\GLmstr\acglfnsc.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx1.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",0)
dim response$(5)*80
response$(1)=str$(10)
fnCmdSet(2)
fnAcs(mat response$,ckey)
pr mat response$
