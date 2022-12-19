! Replace Test\MultiLine
autoLibrary
fnTop(program$)
fnTos
dim resp$(5)*400
fnmultiline(2,5,10,30)
resp$(1)='This is a test'
fnCmdKey('This is a test of dynamic button width',5,0,1)
fnCmdKey('I',1)
ckey=fnAcs(mat resp$)
pr mat resp$
