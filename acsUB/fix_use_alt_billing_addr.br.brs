autoLibrary
on error goto Ertn
!
dim resp$(10)*80
dim z$*10,e$(4)*30,ba$(4)*30
!
fnTop(program$,'Fix Use Alternate Billing Address')
! r: SCREEN1
	fnTos
	pf=34 : ll=32 : width=pf+8
	respc=0
	fnLbl(1,1,"Warning:  Continuing will change the Use/Do Not Use flag",width,2)
	fnLbl(2,1,"on all customer billing addresses. Anyone with a non-blank",width,2)
	fnLbl(3,1,"billing address will be changed to use that billing address.",width,2)
	fnLbl(7,1,"Date of Billing (blank for all):",ll,1)
	fnTxt(7,pf,8,8,1,"1") 
	resp$(1)='' ! cnvrt$("pic(zzzzzz)",d1)
	fnCmdSet(2) 
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	d1=val(resp$(1))
! /r
! r: main loop
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed
	open #hAltBillAddr=fnH: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed 
	changeCount=0
	do
		read #hCustomer,using 'form pos 1,c 10,4*c 30,pos 1854,pd 5.2': z$,mat e$,extra22 eof FINIS
		if ~f or f=d1 and extra22<=0 then
			read #hAltBillAddr,using 'form pos 11,4*c 30',key=z$: mat ba$ nokey abNokey
			if rtrm$(ba$(1)&ba$(2)&ba$(3)&ba$(4))<>"" then 
				extra22=1
				changeCount+=1
				rewrite #hCustomer,using 'form pos 1854,PD 5.2': extra22
			end if
		end if
	abNokey: !
	loop
! /r
FINIS: ! r: pr totals screen
	close #hCustomer: ioerr ignore
	close #hAltBillAddr: ioerr ignore
	fnTos(sn$="Bills-Total") 
	mylen=53 : mypos=mylen+2 
	respc=0
	fnLbl(1,1,"Total Customers Set to use atlernate billing address:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1) 
	resp$(respc+=1)=str$(changeCount)
	fnCmdSet(52) 
	fnAcs(mat resp$,ckey) ! /r
Xit: fnXit
include: ertn
