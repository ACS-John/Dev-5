! Unpaid Invoice Listing (Current)
 
autoLibrary
on error goto Ertn
 
dim dat$*20,vnam$*30,de$*50,fd$*30,ft(3),aa(2),gl(3),ade$*50
dim io1$(2),wrd1$(2),item1$(2)*15
 
fnTop(program$)
fndat (dat$)
fnTos
respc=0
fnLbl(1,40,'',1,1)
fnLbl(1,1,'Order for Printing:',20,1)
item1$(1)='Payee'
item1$(2)='Fund Number'
fnComboA('unpdinv',1,22,mat item1$,tt$)
resp$(respc+=1)=item1$(1)
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then 
	goto Xit 
else if resp$(1)=item1$(1) then 
	fund=1 
else
	fund=2
end if
! FNWAIT
	open #paytrans=4: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,i,k
	open #unpdaloc=8: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr',i,i,k
	open #clwork=10: 'Name=[Q]\CLmstr\CLWORK[acsuserid].h[cno], Size=0, RecL=97, Replace',internal,outIn
READ_PAYTRANS: ! r:
	read #paytrans,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,pos 107,n 8': vn$,iv$,ivd,dd,po$,de$,upa,cde,ddate eof L350
	ivnum+=1 ! UNIQUE Number FOR EACH INVOICE
	restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
READ_UNPDALOC: !
	read #unpdaloc,using 'form pos 1,c 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': unvn$,univ$,mat gl,amt,ade$ eof READ_PAYTRANS
	if vn$<>unun$ and univ$<>iv$ then goto READ_PAYTRANS
	if fund=2 then de$=ade$(1:18)
	write #clwork,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,N 3,N 6,N 3,N 4,n 8': vn$,iv$,ivd,dd,po$,de$(1:18),amt,cde,mat gl,ivnum,ddate
goto READ_UNPDALOC ! /r
L350: !
	close #paytrans: 
	close #unpdaloc: 
	close #clwork:
	upa=0 ! sort ok, sorts a work file
	open #9: 'Name=[temp]\CONTROL,SIZE=0,RecL=128,Replace',internal,output
	write #9,using 'form pos 1,C 128': 'FILE CLWORK[acsuserid].h[cno],[Q]\CLmstr,,[temp]\ADDR,,,,,A,N'
	if fund=2 then : _
		write #9,using 'form pos 1,C 128': 'MASK 74,3,N,A,1,20,C,A,86,4,N,A'
	if fund<>2 then : _
		write #9,using 'form pos 1,C 128': 'MASK 1,20,C,A,86,4,N,A'
	close #9:
	fnFree('[temp]\ADDR')
	execute 'SORT [temp]\CONTROL'
	open #9: 'Name=[temp]\ADDR',i,i
	open #paymstr=13: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,i,k
	open #clwork=10: 'Name=[Q]\CLmstr\CLWORK[acsuserid].h[cno],Shr',i,i,r
	open #glmstr=5: 'Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr',i,i,k
	open #hWork=6: 'Name=[temp]\WORK,RecL=22,Replace',internal,output
	close #hWork:
	fnFree('[temp]\INDX[session]')
L510: !
	execute 'INDEX [temp]\WORK,[temp]\INDX[session],1,12,Replace'
	open #hWork=6: 'Name=[temp]\WORK,KFName=[temp]\INDX[session]',i,outIn,k
	open #hFund=fnH: 'Name=[Q]\CLmstr\FundMstr.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno],Shr',i,i,k
	fnOpenPrn
	vn$='': iv$=''
L560: ! r:
	read #9,using 'form pos 1,PD 3': r4 eof END1
	if r4=0 then goto L560
	read #clwork,using 'form pos 86,N 4',rec=r4: ivnum
	if ivnum=hivnum or hivnum=0 then goto L600 else goto L670
L600: !
	read #clwork,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,N 3,N 6,N 3,N 4,n 8',rec=r4: vn$,iv$,ivd,dd,po$,ade$,amt,cde,mat gl,ivnum,ddate
	upa+=amt
	hivnum=ivnum
	gl$=cnvrt$('PIC(ZZ#)',gl(1))&cnvrt$('PIC(ZZZZZZ)',gl(2))&cnvrt$('PIC(ZZ#)',gl(3))
	if f1=0 then gosub ReadGlControl
	gosub AccumulateGlTotals ! ACCUMULATE G/L TOTALS
goto L560 ! /r
L670: !
	if vn$=hvn$ then goto L730
	if vc<2 then goto L710
	pr #255: tab(97);'__________  __________  __________'
	pr #255,using 'form pos 77,C 18,3*N 12.2': 'Vendor Total',v1,v2,v3
L710: !
	vc=v1=v2=v3=0
	pr #255: pageoflow NEWPGE
L730: !
	vc+=1
	hvn$=vn$
	if fund<>2 then goto L770
	if fund$><gl$(1:3) then gosub TOTALS : pr #255: newpage : gosub ReadGlControl
L770: !
	if cde=1 then 
		p1= 97 : v1+=upa : t1+=upa : ft(1)+=upa 
	else 
		p1=109 : v2+=upa : t2+=upa : ft(2)+=upa
	end if
	v3=v3+upa
	t3=t3+upa : ft(3)=ft(3)+upa
	vnam$=''
	read #paymstr,using L820,key=vn$,release: vnam$ nokey ignore
	L820: form pos 9,c 30
	discdate$=cnvrt$('pic(########)',ddate)
	ddate=val(discdate$(5:8))*100+val(discdate$(3:4))
	pr #255,using 'form pos 1,C 10,C 22,C 12,3*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,pos P1,N 10.2,pos 119,N 12.2': vn$,vnam$(1:20),iv$,ivd,dd,ddate,ade$(1:18),upa,upa pageoflow NEWPGE
	upa=0
	if endcode=1 then goto L1180
goto L600
AccumulateGlTotals: ! r:
	if gl$(3:3)=' ' then gl$(3:3)='0'
	if gl$(12:12)=' ' then gl$(12:12)='0'
	read #hWork,using 'form pos 13,N 10.2',key=gl$: gla nokey L950
	gla+=amt
	rewrite #hWork,using 'form pos 13,N 10.2': gla
	goto L960
	L950: !
	write #hWork,using 'form pos 1,C 12,N 10.2': gl$,amt
	L960: !
return ! /r
 
NEWPGE: pr #255: newpage: gosub HDR : continue
ReadGlControl: ! r:
	fd$=''
	fund$=gl$(1:3)
	read #hFund,using 'form pos 4,C 25',key=fund$: fd$ nokey ignore
goto HDR ! /r
HDR: ! r:
	f1=1
	pr #255,using 'form pos 1,C 8,Cc 86': date$,env$('cnam')
	pr #255,using 'form pos 1,C 8,cc 86': time$,'Unpaid Invoice Listing'
	pr #255,using 'form pos 1,C 4,N 4,Cc 86': 'Page',pg+=1,dat$
	if fund<>2 then fd$=''
	pr #255,using 'form pos 1,Cc 86': fd$ ! ; pr #255: ''
	pr #255: '                                              Invoice     Due     Discount                       Pay Now     Pay Later      Total'
	pr #255: 'Payee  #  Payee Name            Invoice Numb    Date      Date      Date    Description           Amount      Amount         Due'
	pr #255: '________  ____________________  ____________  ________  ________  ________  __________________  __________  __________  __________'
	f1=1
return ! /r
 
END1: ! r:
	if r4=0 then goto Xit
	endcode=1
goto L670 ! /r
L1180: ! r:
	if vc<2 then goto L1210
	pr #255: tab(97);'__________  __________  __________'
	pr #255,using 'form pos 77,C 18,3*N 12.2': 'Vendor Total',v1,v2,v3
	L1210: !
	gosub TOTALS
	pr #255: tab(97);'__________  __________  __________'
	pr #255,using 'form pos 77,C 18,3*N 12.2': 'Final Total',t1,t2,t3
	pr #255: tab(97);'=================================='
	restore #hWork,key>='            ': nokey EO_WORK
	do
		read #hWork,using 'form pos 1,C 12,N 10.2': gl$,gla eof EO_WORK
		if hf$='' or hf$=gl$(1:3) then goto L1300
		gosub TOTF1
		L1300: !
		hf$=gl$(1:3)
		de$=''
		read #glmstr,using 'form pos 13,C 50',key=gl$: de$ nokey ignore
		pr #255,using 'form pos 12,C 14,C 52,N 10.2': gl$,de$,gla pageoflow NEWPGE
		tf1=tf1+gla
	loop
! /r
TOTF1: ! r:
	pr #255: tab(78);'__________'
	if val(hf$)>0 then fd$='Total for Fund #: '&ltrm$(hf$) else fd$='TOTAL'
	pr #255,using 'form pos 12,C 14,C 52,N 10.2': '',fd$,tf1
	pr #255: pageoflow NEWPGE
	tf1=0
return ! /r
 
EO_WORK: ! r:
	gosub TOTF1
	fnClosePrn
	close #hWork,free: ioerr Xit ! /r
Xit: fnXit
 
TOTALS: ! r:
	if fund=2 then 
		pr #255: tab(97);'__________  __________  __________' 
		pr #255,using 'form pos 77,C 18,3*N 12.2': 'Fund   Total',mat ft
	end if
	mat ft=(0)
return ! /r
 
include: ertn
 
