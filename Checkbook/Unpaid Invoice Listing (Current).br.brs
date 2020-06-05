! formerly S:\acsCL\UnPdInv
! Unpaid Invoice Listing (Current)
 
	autoLibrary
	on error goto Ertn
 
	dim dat$*20,vnam$*30,de$*50,fd$*30,ft(3),aa(2),gl(3),ade$*50
	dim io1$(2),wrd1$(2),item1$(2)*15
 
	fnTop(program$)
	fndat (dat$)
	fnTos
	respc=0
	fnLbl(1,40,"",1,1)
	fnLbl(1,1,"Order for Printing:",20,1)
	item1$(1)="Payee"
	item1$(2)="Fund Number"
	fncomboa("unpdinv",1,22,mat item1$,tt$)
	resp$(respc+=1)=item1$(1)
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit else : _
		if resp$(1)=item1$(1) then fund=1 else : _
			fund=2
! FNWAIT
	open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.H[cno],KFName=[Q]\CLmstr\UnPdIdx1.H[cno],Shr",internal,input,keyed
	open #unpdaloc=8: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr",internal,input,keyed
	open #clwork=10: "Name=[Q]\CLmstr\CLWORK"&wsid$&".h[cno], Size=0, RecL=97, Replace",internal,outIn
READ_PAYTRANS: !
	read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,pos 107,n 8': vn$,iv$,ivd,dd,po$,de$,upa,cde,ddate eof L350
	ivnum+=1 ! UNIQUE Number FOR EACH INVOICE
	restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
READ_UNPDALOC: !
	read #unpdaloc,using 'Form pos 1,c 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': unvn$,univ$,mat gl,amt,ade$ eof READ_PAYTRANS
	if vn$<>unun$ and univ$<>iv$ then goto READ_PAYTRANS
	if fund=2 then de$=ade$(1:18)
	write #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,N 3,N 6,N 3,N 4,n 8': vn$,iv$,ivd,dd,po$,de$(1:18),amt,cde,mat gl,ivnum,ddate
	goto READ_UNPDALOC
L350: close #paytrans: : close #unpdaloc: : close #clwork:
	upa=0 ! sort ok, sorts a work file
	open #9: "Name="&env$('temp')&'\'&"CONTROL,SIZE=0,RecL=128,Replace",internal,output
	write #9,using 'Form POS 1,C 128': "FILE CLWORK"&wsid$&".H[cno],[Q]\CLmstr,,"&env$('temp')&'\'&"ADDR,,,,,A,N"
	if fund=2 then : _
		write #9,using 'Form POS 1,C 128': "MASK 74,3,N,A,1,20,C,A,86,4,N,A"
	if fund<>2 then : _
		write #9,using 'Form POS 1,C 128': "MASK 1,20,C,A,86,4,N,A"
	close #9:
	fnFree(env$('temp')&'\'&"ADDR")
	execute "SORT "&env$('temp')&'\'&"CONTROL"
	open #9: "Name="&env$('temp')&'\'&"ADDR",internal,input
	open #paymstr=13: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,input,keyed
	open #clwork=10: "Name=[Q]\CLmstr\CLWORK"&wsid$&".h[cno],Shr",internal,input,relative
	open #glmstr=5: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,input,keyed
	open #work=6: "Name="&env$('temp')&'\'&"WORK,SIZE=0,RecL=22,Replace",internal,output
	close #work:
	fnFree("INDX."&wsid$)
L510: execute "INDEX "&env$('temp')&'\'&"WORK,"&env$('temp')&'\'&"INDX,1,12,Replace"
	open #work=6: "Name="&env$('temp')&'\'&"WORK,KFName="&env$('temp')&'\'&"INDX",internal,outIn,keyed
	open #fundmstr=7: "Name=[Q]\CLmstr\FundMstr.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno],Shr",internal,input,keyed
	fnopenprn
	vn$="": iv$=""
L560: read #9,using 'FORM POS 1,PD 3': r4 eof END1
	if r4=0 then goto L560
	read #clwork,using 'Form Pos 86,N 4',rec=r4: ivnum
	if ivnum=hivnum or hivnum=0 then goto L600 else goto L670
L600: read #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,N 3,N 6,N 3,N 4,n 8',rec=r4: vn$,iv$,ivd,dd,po$,ade$,amt,cde,mat gl,ivnum,ddate
	upa+=amt
	hivnum=ivnum
	gl$=cnvrt$("PIC(ZZ#)",gl(1))&cnvrt$("PIC(ZZZZZZ)",gl(2))&cnvrt$("PIC(ZZ#)",gl(3))
	if f1=0 then gosub L990
	gosub L890 ! ACCUMULATE G/L TOTALS
	goto L560
L670: if vn$=hvn$ then goto L730
	if vc<2 then goto L710
	pr #255: tab(97);"__________  __________  __________"
	pr #255,using 'FORM POS 77,C 18,3*N 12.2': "Vendor Total",v1,v2,v3
L710: vc=v1=v2=v3=0
	pr #255: pageoflow NEWPGE
L730: vc+=1
	hvn$=vn$
	if fund<>2 then goto L770
	if fund$><gl$(1:3) then gosub TOTALS : pr #255: newpage : gosub L990
L770: if cde=1 then p1=97: v1=v1+upa: t1=t1+upa : ft(1)=ft(1)+upa else p1=109: v2=v2+upa : t2=t2+upa : ft(2)=ft(2)+upa
	v3=v3+upa
	t3=t3+upa : ft(3)=ft(3)+upa
	vnam$=""
	read #paymstr,using L820,key=vn$,release: vnam$ nokey L830
L820: form pos 9,c 30
L830: discdate$=cnvrt$("pic(########)",ddate)
	ddate=val(discdate$(5:8))*100+val(discdate$(3:4))
	pr #255,using 'FORM POS 1,C 10,C 22,C 12,3*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS P1,N 10.2,POS 119,N 12.2': vn$,vnam$(1:20),iv$,ivd,dd,ddate,ade$(1:18),upa,upa pageoflow NEWPGE
	upa=0
	if endcode=1 then goto L1180
	goto L600
L890: if gl$(3:3)=" " then gl$(3:3)="0"
	if gl$(12:12)=" " then gl$(12:12)="0"
	read #work,using 'FORM POS 13,N 10.2',key=gl$: gla nokey L950
	gla+=amt
	rewrite #work,using 'FORM POS 13,N 10.2': gla
	goto L960
L950: write #work,using 'FORM POS 1,C 12,N 10.2': gl$,amt
L960: return
 
NEWPGE: pr #255: newpage: gosub HDR : continue
L990: fd$=""
	fund$=gl$(1:3)
	read #fundmstr,using 'FORM POS 4,C 25',key=fund$: fd$ nokey HDR
HDR: f1=1
	pr #255,using 'FORM POS 1,C 8,Cc 86': date$,env$('cnam')
	pr #255,using 'FORM POS 1,C 8,cc 86': time$,"Unpaid Invoice Listing"
	pr #255,using 'FORM POS 1,C 4,N 4,Cc 86': "Page",pg+=1,dat$
	if fund<>2 then fd$=""
	pr #255,using 'FORM POS 1,Cc 86': fd$ ! ; pr #255: ""
	pr #255: "                                              Invoice     Due     Discount                       Pay Now     Pay Later      Total"
	pr #255: "Payee  #  Payee Name            Invoice Numb    Date      Date      Date    Description           Amount      Amount         Due"
	pr #255: "________  ____________________  ____________  ________  ________  ________  __________________  __________  __________  __________"
	f1=1
return
 
END1: !
	if r4=0 then goto Xit
	endcode=1
	goto L670
L1180: if vc<2 then goto L1210
	pr #255: tab(97);"__________  __________  __________"
	pr #255,using 'FORM POS 77,C 18,3*N 12.2': "Vendor Total",v1,v2,v3
L1210: gosub TOTALS
	pr #255: tab(97);"__________  __________  __________"
	pr #255,using 'FORM POS 77,C 18,3*N 12.2': "Final Total",t1,t2,t3
	pr #255: tab(97);"=================================="
	restore #work,key>="            ": nokey EO_WORK
READ_WORK: !
	read #work,using 'FORM POS 1,C 12,N 10.2': gl$,gla eof EO_WORK
	if hf$="" or hf$=gl$(1:3) then goto L1300
	gosub TOTF1
L1300: hf$=gl$(1:3)
	de$=""
	read #glmstr,using 'FORM POS 13,C 50',key=gl$: de$ nokey L1330
L1330: pr #255,using 'FORM POS 12,C 14,C 52,N 10.2': gl$,de$,gla pageoflow NEWPGE
	tf1=tf1+gla
	goto READ_WORK
TOTF1: pr #255: tab(78);"__________"
	if val(hf$)>0 then fd$="Total for Fund #: "&ltrm$(hf$) else fd$="TOTAL"
	pr #255,using 'FORM POS 12,C 14,C 52,N 10.2': "",fd$,tf1
	pr #255: pageoflow NEWPGE
	tf1=0
return
 
EO_WORK: gosub TOTF1
	fncloseprn
	close #work,free: ioerr Xit
Xit: fnXit
 
TOTALS: !
	if fund=2 then : _
		pr #255: tab(97);"__________  __________  __________" : _
		pr #255,using 'FORM POS 77,C 18,3*N 12.2': "Fund   Total",mat ft
	mat ft=(0)
return
 
include: Ertn
 
