
autoLibrary
on error goto Ertn

fnTop(program$)
dim id$(6)*40
dim fil$(6)*18
dim idx$(6)*18
id$(1)=' 1. Balance Sheet File'               	: fil$(1)='ACGLFNSB.h[cno]' : idx$(1)='agfsidx4.h[cno]'
id$(2)=' 2. Income Statement File'            	: fil$(2)='ACGLFNSI.h[cno]' : idx$(2)='agfsidx3.h[cno]'
id$(3)=' 3. Fund Statement / Cash Flow File' 	: fil$(3)='ACGLFNSF.h[cno]' : idx$(3)='agfsidx5.h[cno]'
id$(4)=' 4. Secondary Balance Sheet File'    	: fil$(4)='ACGLFNSC.h[cno]' : idx$(4)='agfsidx1.h[cno]'
id$(5)=' 5. Secondary Income Statement File' 	: fil$(5)='ACGLFNSJ.h[cno]' : idx$(5)='agfsidx2.h[cno]'
id$(6)=' 6. Secondary Fund / Cash Flow File' 	: fil$(6)='ACGLFNSG.h[cno]' : idx$(6)='agfsidx6.h[cno]'

! r: build layout
	dim lbl$(21)*38,tln(21),p$(21)*160,fltyp$(21),sln(21),mask(21),sp(21),c$(21,8)*40
	ic=0
	! label text                           	: field type      : storage len : mask        : spos      : disp Len
	lbl$(ic+=1)='F/S #'                  	: fltyp$(ic)='c' : sln(ic)= 5 : mask(ic)= 0 : sp(ic)= 1 : tln(ic)= 5
	lbl$(ic+=1)='Description'            	: fltyp$(ic)='c' : sln(ic)=50 : mask(ic)= 0 : sp(ic)= 6 : tln(ic)=50
	lbl$(ic+=1)='Type of Entry'         	: fltyp$(ic)='C' : sln(ic)= 1 : mask(ic)= 0 : sp(ic)=56 : tln(ic)= 1
	lbl$(ic+=1)='Starting Print'        	: fltyp$(ic)='N' : sln(ic)= 2 : mask(ic)=30 : sp(ic)=57 : tln(ic)= 2
	lbl$(ic+=1)='Lines to skip'         	: fltyp$(ic)='N' : sln(ic)= 2 : mask(ic)=30 : sp(ic)=59 : tln(ic)= 2
	lbl$(ic+=1)='Dollar Sign'           		: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=61 : tln(ic)= 1
	lbl$(ic+=1)='Underlines'             	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=62 : tln(ic)= 1
	lbl$(ic+=1)='Reverse Sign'           	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=63 : tln(ic)= 1
	lbl$(ic+=1)='B/S Column'             	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=64 : tln(ic)= 1
	lbl$(ic+=1)='Print Accumulator'     	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=65 : tln(ic)= 1
	lbl$(ic+=1)='Clr 1'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=66 : tln(ic)= 1
	lbl$(ic+=1)='Clr 2'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=67 : tln(ic)= 1
	lbl$(ic+=1)='Clr 3'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=68 : tln(ic)= 1
	lbl$(ic+=1)='Clr 4'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=69 : tln(ic)= 1
	lbl$(ic+=1)='Clr 5'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=70 : tln(ic)= 1
	lbl$(ic+=1)='Clr 6'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=71 : tln(ic)= 1
	lbl$(ic+=1)='Clr 7'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=72 : tln(ic)= 1
	lbl$(ic+=1)='Clr 8'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=73 : tln(ic)= 1
	lbl$(ic+=1)='Clr 9'                  	: fltyp$(ic)='N' : sln(ic)= 1 : mask(ic)=30 : sp(ic)=74 : tln(ic)= 1
	lbl$(ic+=1)='I/C % Base'             	: fltyp$(ic)='N' : sln(ic)= 3 : mask(ic)=30 : sp(ic)=75 : tln(ic)= 3
	lbl$(ic+=1)='Cost Center'            	: fltyp$(ic)='N' : sln(ic)= 5 : mask(ic)=30 : sp(ic)=78 : tln(ic)= 5
! /r

do
	fnTos
	mylen=20: mypos=mylen+3 : right=1
	fnFra(1,1,6,60,'Financial Statement Choices','Choose the financial statement to work with.')
	fnOpt(1,2,id$(1),0,1)
	resp$(1)='True'
	fnOpt(2,2,id$(2) ,0,1)
	resp$(2)='False'
	fnOpt(3,2,id$(3),0,1)
	resp$(3)='False'
	fnOpt(4,2,id$(4),0,1)
	resp$(4)='False'
	fnOpt(5,2,id$(5),0,1)
	resp$(5)='False'
	fnOpt(6,2,id$(6),0,1)
	resp$(6)='False'
	fnCmdKey('&Next',1,1,0,'Access the chosen financial statement design..')
	fnCmdKey('&Cancel',5,1,0,'Return to main menu.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)='True' then selection=1
	if resp$(2)='True' then selection=2
	if resp$(3)='True' then selection=3
	if resp$(4)='True' then selection=4
	if resp$(5)='True' then selection=5
	if resp$(6)='True' then selection=6
	open #1: 'Name=[Q]\GLmstr\'&fil$(selection)&',KFName=[Q]\GLmstr\'&idx$(selection)&',Use,RecL=83,KPs=1,KLn=5,Shr',internal,outIn,keyed
	fnHamster('Acglfnsb',mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
	close #1: ioerr ignore
	fnIndex('[Q]\GLmstr\'&fil$(selection),'[Q]\GLmstr\'&idx$(selection),'1 5')
loop

Xit: fnXit

include: ertn

