! Replace Test\wait

	autoLibrary

	dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2

	fnTop(program$,"Test 5 second transition between screens")

	pr 'just before tos - '&time$
	fnTos
	chdr$(1)="Account" : chdr$(2)="Name" : _
	chdr$(3)="Address" : chdr$(4)="City" : _
	chdr$(5)="State" : chdr$(6)="Zip"
	cm$(1)="80" : cm$(2)="80" : cm$(3)="80" : cm$(4)="80" : _
	cm$(5)="80" : cm$(6)="80"
	fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,0,repl=1)
	open #1: "Name=Sample\FlexTest,RecL=200,Use",internal,outIn
READ_1: ! : _
	read #1,using "Form POS 1,C 10,2*C 30,C 20,C 2,C 5": mat item$ eof EOF1
	fnflexadd1(mat item$)
	goto READ_1
EOF1: close #1:
	fnCmdKey('Ok-1',1,1,0)
	fnAcs2(mat resp$,ckey)
	pr 'just after return from fnAcs - '&time$
 
	sleep(5) : pr "sleep for 5 seconds"
	pr 'just before tos - '&time$
	fnTos(sn$="S-2")
	fnCmdKey('Ok-2',1,1,0)
	fnAcs2(mat resp$,ckey)
	pr 'just after return from fnAcs - '&time$
 
	sleep(5) : pr "sleep for 5 seconds"
	pr 'just before tos - '&time$
	fnTos(sn$="S-3")
	fnCmdKey('Ok-3',1,1,0)
	fnAcs2(mat resp$,ckey)
	pr 'just after return from fnAcs - '&time$
 
	sleep(5) : pr "sleep for 5 seconds"
	pr 'just before tos - '&time$
	fnTos(sn$="S-4")
	fnCmdKey('Ok-4',1,1,0)
	fnAcs2(mat resp$,ckey)
	pr 'just after return from fnAcs - '&time$
 
	stop
