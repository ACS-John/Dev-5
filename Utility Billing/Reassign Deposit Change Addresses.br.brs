! Replace S:\acsUB\DepReas
! -- Reassign Deposit Change Addresses
 
	autoLibrary
	on error goto Ertn
 
dim msgline$(3)*60,ta(2),message$*40

 
	fnTop(program$)
 
MAIN: !
	msgline$(1)="No other users may be using the Deposit file"
	msgline$(2)="while this option is running.  Do you want to run"
	msgline$(3)="Reassign Deposit Change Addresses now?"
	fnmsgbox(mat msgline$,resp$,'',49)
	if uprc$(resp$)=uprc$("CANCEL") then goto Xit
 
	open #1: "Name=[Q]\UBmstr\Deposit1.h[cno],KFName=[Q]\UBmstr\DepIdx1.h[cno]",internal,outIn,keyed ioerr MAIN
	open #2: 'Name=[Q]\UBmstr\Deposit2.h[cno],KFName=[Q]\UBmstr\Deposit2Index.h[cno],Shr,Use,RecL=73,KPs=1,KLn=10',internal,outIn,keyed ! "Name=[Q]\UBmstr\Deposit2.h[cno]",i,outi,r ioerr MAIN
 
TOP: !
	read #1,using "Form POS 11,2*PD 3": mat ta eof L240
	rewrite #1,using "Form POS 11,2*PD 3": 0,0
goto TOP
 
L240: !
	lr2=lrec(2)
	if lr2=0 then goto Xit
	rewrite #2,using "Form POS 71,PD 3",rec=1: lr2
	for j=1 to lr2
		read #2,using "Form POS 1,C 10,POS 71,PD 3",rec=j: k$,nta noRec L350
		read #1,using "Form POS 11,2*PD 3",key=k$: mat ta nokey L350
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then
			rewrite #2,using "Form POS 71,PD 3",rec=ta(2): j
		end if
		ta(2)=j
		rewrite #1,using "Form POS 11,2*PD 3",key=k$: mat ta
		rewrite #2,using "Form POS 71,PD 3",rec=j: 0
		L350: !
	next j
goto Xit
 
Xit: fnXit
 
include: ertn
 
