! Replace S:\acsCL\ckLstV
! Check Listing by Vendor (Transaction List: sort by Vendor)

autoLibrary
on error goto Ertn
dim cnam$*40,dat$*20 ! CNO
dim tr$(5)*35 ! TRMstr
dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,ta(2) ! PayMstr
fntop(program$,'Check Listing by Payee')
right=1
open #trmstr=22: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
open #paymstr:=1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed 
fnTos
respc=0 : mylen=25 : mypos=mylen+2
fnLbl(1,40,"",1,1)
fnLbl(1,1,"Beginning Date:",mylen,right)
fnTxt(1,mypos,8,0,1,"ccyymmdd") 
resp$(respc+=1)=""
fnLbl(2,1,"Ending Date:",mylen,right)
fnTxt(2,mypos,8,0,1,"ccyymmdd") 
resp$(respc+=1)=""
fnCmdSet(3)
fnAcs2(mat resp$,ckey)
if ckey=5 then goto XIT
date1=val(resp$(1)) 
date2=val(resp$(2))
fnopenprn
gosub HDR
goto BODY
HDR: ! r: Page Heading
	pr #255,using 'Form POS 1,CC 80': cnam$
	pr #255,using 'Form POS 1,CC 80': "Company Number [cno]"
	pr #255,using 'Form POS 1,CC 80': "Check Listing By Vendor"
	pr #255,using 'Form Pos 1,Cc 80': "For the Date Range Starting "&cnvrt$("pic(zzzz/zz/zz)",date1)&" and Ending "&cnvrt$("pic(zzzz/zz/zz)",date2)
	pr #255: ""
	pr #255,using 'Form Pos 1,C 80': "Chk/Ref# Date   Amount      Payee No Name/Description"
	pr #255,using 'Form Pos 1,C 80': "________ ______ ___________ ________ ___________________________________"
return ! /r
PGOF: ! r: 
	pr #255: newpage
	gosub HDR
continue ! /r
BODY: ! r: 
	read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof LAS 
	tr$(3)=str$(tr3)
	if date1<>0 and fndate_mmddyy_to_ccyymmdd(val(tr$(2)))<date1 then goto BODY
	if date2<>0 and fndate_mmddyy_to_ccyymmdd(val(tr$(2)))>date2 then goto BODY
	if tcde<>1 then goto BODY ! checks only
	if tr$(4)<>vn$ and vn$<>"" then gosub TOTALS
	if tr$(4)<>vn$ then gosub SUBHEADING
	pr #255,using 'Form POS 1,C 8,X 1,C 6,N 12.2,X 1,C 8,X 1,C 35': tr$(1),tr$(2),val(tr$(3)),tr$(4) pageoflow PGOF
	total1+=val(tr$(3))
goto BODY ! /r
SUBHEADING: ! r: 
	ytdp=typ=ta(1)=ta(2)=0 : vn$=nam$=ad1$=ad2$=csz$=ss$=ph$=""
	read #paymstr,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12',key=tr$(4): vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$ nokey PR_NOKEY
	pr #255,using 'Form POS 1,C 80': "Vendor: "&trim$(vn$)&". "&nam$ pageoflow PGOF : goto PAST_PR_NOKEY
	PR_NOKEY: ! 
	pr #255,using 'Form POS 1,C 80': "Vendor: "&trim$(tr$(4))&". (This Vendor has been Deleted)" pageoflow PGOF
	PAST_PR_NOKEY: ! 
	total1=0
return ! /r
TOTALS: ! r: 
	pr #255,using 'Form POS 1,C 10,N 17.2': "Totals:",total1 
	pr #255: ""
	total1=0
return ! /r
LAS: ! r: 
	gosub TOTALS
	fncloseprn
goto XIT ! /r
XIT: fnxit
include: Ertn