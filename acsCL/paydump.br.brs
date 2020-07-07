! Replace S:\acsCL\PayDump
! Remove Payee Records
 
	autoLibrary
	on error goto Ertn
 
	dim nam$*30,cnam$*40,dat$*20,gl(3),tr$(5)*35,cap$*128
 
	fnTop(program$,cap$="Remove Payee Records")
	cancel=99 : right=1
	fncno(cno,cnam$) : _
	fndat(dat$)
 
	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input,relative: read #20,using 'Form POS 150,2*N 1',rec=1: mat d : _
	close #20:
	open #trmstr2=22: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx2.H[cno],Shr",internal,input,keyed
	open #paymstr1=1: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed
	open #paymstr2=2: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx2.H[cno],Shr",internal,outIn,keyed
	open #payeeglbreakdown:=fngethandle: "Name=[Q]\CLmstr\PayeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\PayeeGLBkdidx.h[cno],Shr",internal,outIn,keyed
	fnTos(sn$="PayDump") : _
	respc=0 : mylen=21 : mypos=mylen+2
	fnLbl(1,1,"Oldest retained Date:",mylen,right)
	fnTxt(1,mypos,10,0,1,"1003",0,"This program will dump payee records who have not received a check since a certain date.") : _
	resp$(respc+=1)=str$(date('ccyymmdd')-50000)
	fnLbl(1,46,"",1,1)
	fnCmdSet(2) : _
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit else : _
		olddate=val(resp$(1))
	fnopenprn
	gosub HDR
! 
READ_PAYMSTR1: !
	read #paymstr1,using 'Form POS 1,C 8,C 30': vn$,nam$ eof DONE
	restore #trmstr2,search>=vn$: nokey PRINT_IT
READ_TRMSTR2: !
	read #trmstr2,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof READ_PAYMSTR1 : tr$(3)=str$(tr3)
	if vn$><tr$(4) then goto PRINT_IT : _
		! moved thru all checks without finding a check with date : _
		! later than one entered above
	lastdate=val(tr$(2)) conv READ_TRMSTR2
	if fndate_mmddyy_to_ccyymmdd(lastdate)>olddate then goto READ_PAYMSTR1 : _
		! keep this vendor recored
	goto READ_TRMSTR2
 
PRINT_IT: !
	pr #255,using "Form POS 1,C 8,X 3,C 30": vn$,nam$ pageoflow NEWPGE
	delete #paymstr1,key=vn$: nokey L410
	gosub REMOVE_FROM_PAYEEGLBREAKDOWN
L410: goto READ_PAYMSTR1
 
REMOVE_FROM_PAYEEGLBREAKDOWN: ! uses VN$
	restore #payeeglbreakdown,key>=vn$: nokey OUTTA_PGB_LOOP
READ_PAYEEGLBREAKDOWN: !
	read #payeeglbreakdown,using 'Form Pos 1,C 8': readvn$ eof OUTTA_PGB_LOOP
	if readvn$=vn$ then : _
		delete #payeeglbreakdown: : _
		goto READ_PAYEEGLBREAKDOWN else : _
		goto OUTTA_PGB_LOOP
OUTTA_PGB_LOOP: !
return
 
DONE: !
	fncloseprn : _
	goto Xit
 
Xit: fnXit
 
NEWPGE: pr #255: newpage : gosub HDR : continue
 
HDR: !
	pr #255,using 'Form POS 1,Cc 80': cnam$ : _
	pr #255,using 'Form POS 1,Cc 80': cap$ : _
	pr #255,using 'Form POS 1,Cc 80': dat$
return
 
include: Ertn No
 
