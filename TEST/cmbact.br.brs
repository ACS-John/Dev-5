autoLibrary
dim response$(2)*80
 
fnTop(program$)
fnTos
fncmbact(1,1)
fnCmdSet(2)
fnAcs2(mat response$,ckey)
pr response$(1)
 
key$=response$(1)(32:41)
pr "the key is "&key$
