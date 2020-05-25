! Replace Test\Flex
	autoLibrary
	dim resp$(22)*100
	fnTop(program$,cap$="Test Flex")
	dim item$(6)*30,prg$*30,chdr$(6)*20,cm$(6)*2
SCREEN1: !
	fnTos("S-Flex")
! use_old=1 : replace_old=0 : select_by_row=1
! chdr$(1)="AccountX" : chdr$(2)="Name" : _
	! chdr$(3)="Address" : chdr$(4)="City" : _
	! chdr$(5)="State" : chdr$(6)="Zip"
! cm$(1)="81" : cm$(2)="81" : cm$(3)="80" : cm$(4)="80" : _
	! cm$(5)="80" : cm$(6)="80"
! fnflexinit1('flex_test',1,1,10,50,mat chdr$,mat cm$,select_by_row,replace_old)
	mat chdr$(7) : mat cmask$(7) : mat item$(7)
	chdr$(1)='Rec'
	chdr$(2)='GL Account'
	chdr$(3)='Amount'
	chdr$(4)='Description'
	chdr$(5)='Invoice'
	chdr$(6)='PO Number'
	chdr$(7)='PC'
	mat cmask$=("")
	cmask$(1)='30'
	cmask$(2)=''
	cmask$(3)='10'
	cmask$(5)=''
	fnflexinit1('TrAlloc-tran2',17,1,4,90,mat chdr$,mat cmask$,1)
	open #h_tralloc=fngethandle: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,input,keyed
	do
		read #h_tralloc,using 'Form Pos 1,C 11,C 12,pd 5.2,C 30,G 6,X 3,C 12,G 1': newkey$,item$(2),tmp,item$(4),item$(5),item$(6),item$(7) eof EOF1
		allocationstotal+=tmp
		item$(1)=str$(rec(h_tralloc))
		item$(3)=str$(tmp)
		fnflexadd1(mat item$)
	loop
EOF1: close #h_tralloc:
	fnCmdKey('Ok',1,1,0)
	fnCmdKey('Cancel',99,0,1)
	fnAcs("S-Flex",0,mat resp$,ck)
	pr "returned response is "&resp$(1)
	pr "Press Enter to continue"
! input fields "1,1,C 1,AE,N": pause$
	goto SCREEN1
Xit: fnXit
