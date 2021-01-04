! pr Report of Invoices Due By Selected Dates
autoLibrary
on error goto Ertn

dim vnam$*30
dim ade$*50
dim t2(10)
dim d2(10)

fnTop(program$,"Report of Invoices Due By Selected Dates")
dim dat$*20
fndat(dat$)
gosub Ask
fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\Unpdidx2.h[cno]','31/27/1 2/4/26') ! index in year,monthday,reference
open #paymstr=fnH: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,input,keyed 
open #paytrans=fnH: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\Unpdidx2.h[cno],Shr",internal,input,keyed 
fnopenprn
gosub Hdr
vn$=iv$=""
do
	read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,N 10.2,N 1,N 2,G 8,G 6': vn$,iv$,ivd,dd,po$,ade$,amt,cde eof Finis
	L230: !
	if fndate_mmddyy_to_ccyymmdd(dd)>d2(wd2) then goto TOT1
	vnam$=""
	read #paymstr,using 'Form POS 9,C 30',key=vn$,release: vnam$ nokey ignore
	pr #255,using 'Form POS 1,C 10,C 32,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS 99,N 10.2': vn$, vnam$,iv$,ivd,dd,ade$(1:18),amt pageoflow NEWPGE
	t1+=amt
loop
Finis: ! r:
	if Finis=1 then goto L630
	Finis=1 : goto TOT1
	t2=0
	L630: !
	for j=1 to d2
		pr #255,using L650: "      Due by "&cnvrt$("PIC(####/##/##)",d2(j)),t2(j)
		L650: form pos 10,c 23,pic(zzz,zzz,zzz,zzz.##bcr),skip 1
		t2=t2+t2(j)
	next j
	pr #255: tab(37);"______________"
	pr #255,using L650: "Total Due by "&cnvrt$("PIC(####/##/##)",d2(d2)),t2
	fncloseprn
goto Xit ! /r
Xit: fnXit
NEWPGE: pr #255: newpage: gosub HDR : continue 
TOT1: ! r: pr DATE TOTAL
	pr #255,using 'Form POS 99,"----------",SKIP 1,POS 76,C 23,N 10.2': "Total Due by "&cnvrt$("PIC(####/##/##)",d2(wd2)),t1 
	pr #255: ""
	t2(wd2)=t1
	if wd2=d2 or Finis=1 then goto Finis
	wd2+=1
	if wd2>d2 or fndate_mmddyy_to_ccyymmdd(dd)>d2(d2) then wd2=d2 : goto Finis
	goto L420 ! DONT SKIP BETWEEN DATES
	pr #255: newpage
	gosub HDR
L420: !
goto L230 ! /r
HDR: ! r: 
	pr #255,using 'form pos 1,c 8,pos 20,cc 40': date$,env$('cnam')
	pr #255,using 'form pos 1,c 8,pos 20,cc 40': time$,"Invoices Due by Selected Dates"
	pr #255,using 'form pos 1,c 4,n 4,pos 20,cc 40': "Page",pg+=1,dat$
	pr #255,using 'form pos 20,cc 40,skip 2': fd$
	pr #255: "                                                        Invoice     Due                           Due By"
	pr #255: "Payee No  Payee Name                      Invoice Numb    Date      Date    Description         "&cnvrt$("PIC(####/##/##)",d2(wd2))
	pr #255: "________  ______________________________  ____________  ________  ________  __________________  ____________"
return ! /r

Ask: ! r:
	fnTos
	respc=0
	fnLbl(1,40,"",1,1)
	fnLbl(1,1,"1st Due By Date:",25,1)
	fnTxt(1,27,10,0,1,"3",0,"Normally these would be dates in the future such as how much will be due by the 15th and the 30th.  Use ccyymmdd format" ) 
	resp$(respc+=1)=""
	fnLbl(2,1,"2nd Due by Date:",25,1)
	fnTxt(2,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(3,1,"3nd Due by Date:",25,1)
	fnTxt(3,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(4,1,"4th Due By Date:",25,1)
	fnTxt(4,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(5,1,"5th Due by Date:",25,1)
	fnTxt(5,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(6,1,"6th Due by Date:",25,1)
	fnTxt(6,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(7,1,"7th Due By Date:",25,1)
	fnTxt(7,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(8,1,"8th Due by Date:",25,1)
	fnTxt(8,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(9,1,"9th Due by Date:",25,1)
	fnTxt(9,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnLbl(10,1,"10th Due By Date:",25,1)
	fnTxt(10,27,10,0,1,"3") 
	resp$(respc+=1)=""
	fnCmdSet(2): fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	for j=1 to 10
		d2(j)=val(resp$(j))
	next j
	for j=1 to 10
		if j=1 then goto L1050
		if d2(j)=0 then d2=j-1 : goto L1060 else d2=j
		if d2(j)<d2(j-1) then goto ASK ! probably message box (dates out of order)
	L1050: next j
	L1060: wd2=1
return ! /r
include: ertn

