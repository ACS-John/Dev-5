autoLibrary

dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2

fnTop(prg$="Sample\Flex",cap$="Sample Flex")

fnTos
chdr$(1)="Account" : chdr$(2)="Name"
chdr$(3)="Address" : chdr$(4)="City"
chdr$(5)="State"   : chdr$(6)="Zip"
cm$(1)="80" : cm$(2)="80" : cm$(3)="80" : cm$(4)="80" 
cm$(5)="80" : cm$(6)="80"
fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,0,repl=1)
open #1: "Name=Sample\FlexTest,RecL=200,Use",internal,outIn
READ_1: ! 
read #1,using "form pos 1,C 10,2*C 30,C 20,C 2,C 5": mat item$ eof EOF1
fnflexadd1(mat item$)
goto READ_1
EOF1: close #1:
fnCmdKey('Ok',1,1,0)
fnCmdKey('Cancel',99,0,1)
ckey=fnAcs(mat resp$)
pr "returned response is "&resp$(1) 
pr "Press Enter to continue"
fnpause
stop
