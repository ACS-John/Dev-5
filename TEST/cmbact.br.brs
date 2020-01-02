library 'S:\Core\Library': fncmbact,fnAcs2,fnTos,fnCmdSet,fntop
dim response$(2)*80

fntop(program$)
fnTos
fncmbact(1,1)
fnCmdSet(2)
fnAcs2(mat response$,ckey)
pr response$(1)

key$=response$(1)(32:41)
pr "the key is "&key$
