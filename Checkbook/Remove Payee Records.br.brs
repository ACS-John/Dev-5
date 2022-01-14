autoLibrary
on error goto Ertn

dim nam$*30
dim gl(3),tr$(5)*35

fnTop(program$)
dim dat$*20
fndat(dat$)

open #20: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,i,r: read #20,using 'form pos 150,2*N 1',rec=1: mat d
close #20:
open #trmstr2=22: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr',i,i,k
open #paymstr1=1: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',internal,outIn,keyed
open #paymstr2=2: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr',internal,outIn,keyed
open #payeeglbreakdown=fnH: 'Name=[Q]\CLmstr\PayeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\PayeeGLBkdidx.h[cno],Shr',internal,outIn,keyed
fnTos
respc=0 : mylen=21 : mypos=mylen+2
fnLbl(1,1,'Oldest Retained Date:',mylen,1)
fnTxt(1,mypos,10,0,1,'1003',0,'This program will dump payee records who have not received a check since a certain date.')
resp$(respc+=1)=str$(date('ccyymmdd')-50000)
fnLbl(1,46,'',1,1)
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit else olddate=val(resp$(1))
fnopenprn
gosub HDR

READ_PAYMSTR1: !
	read #paymstr1,using 'form pos 1,C 8,C 30': vn$,nam$ eof DONE
	restore #trmstr2,search>=vn$: nokey PRINT_IT
READ_TRMSTR2: !
	read #trmstr2,using 'form pos 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof READ_PAYMSTR1 : tr$(3)=str$(tr3)
	if vn$><tr$(4) then goto PRINT_IT 		! moved thru all checks without finding a check with date  later than one entered above
	lastdate=val(tr$(2)) conv READ_TRMSTR2
	if fndate_mmddyy_to_ccyymmdd(lastdate)>olddate then goto READ_PAYMSTR1		! keep this vendor recored
goto READ_TRMSTR2

PRINT_IT: !
	pr #255,using 'form pos 1,C 8,X 3,C 30': vn$,nam$ pageoflow NEWPGE
	delete #paymstr1,key=vn$: nokey L410
	gosub REMOVE_FROM_PAYEEGLBREAKDOWN
	L410: !
goto READ_PAYMSTR1

REMOVE_FROM_PAYEEGLBREAKDOWN: ! uses VN$
	restore #payeeglbreakdown,key>=vn$: nokey OUTTA_PGB_LOOP
READ_PAYEEGLBREAKDOWN: !
	read #payeeglbreakdown,using 'form pos 1,C 8': readvn$ eof OUTTA_PGB_LOOP
	if readvn$=vn$ then
		delete #payeeglbreakdown:
		goto READ_PAYEEGLBREAKDOWN 
	else
		goto OUTTA_PGB_LOOP
	end if
OUTTA_PGB_LOOP: !
return

DONE: !
	fncloseprn
	goto Xit

Xit: fnXit

NEWPGE: pr #255: newpage : gosub HDR : continue

HDR: !
	pr #255,using 'form pos 1,Cc 80': env$('cnam')
	pr #255,using 'form pos 1,Cc 80': env$('program_caption')
	pr #255,using 'form pos 1,Cc 80': dat$
return

include: ertn No

