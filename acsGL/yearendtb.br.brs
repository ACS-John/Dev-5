! Replace S:\acsGL\YearendTB
! reprint trial balance for last year end
 
	autoLibrary
	on error goto Ertn
 
	dim cnam$*40,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
	dim a$(9)*3,cogl$(2)*12,u$*12,c$*5,d(2),ta(2)
	dim wrddetail$(2),p$(20)*50,resp$(10)*80,bp(13),cap$*128
 
	right=1
	fnTop(program$,cap$="Reprint Year End Trial Balance")
	fncno(cno,cnam$)
	open #20: "Name=[Q]\GLmstr\Company.h[cno]",i,i,r  : _
	read #20,using 'form pos 150,2*N 1',rec=1: d(1),d(2) : _
	read #20,using 'form pos 152,2*C 12',rec=1: mat cogl$ : _
	close #20:
	a$(1)="C/D" : a$(2)="C/R" : a$(3)="ADJ" : _
	a$(4)="A/P" : a$(5)="PR" : a$(6)="A/R" : _
	a$(7)="S/J" : a$(8)="P/J" : a$(9)=" "
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",i,i,k
	goto START_REPORT
SCREEN1: !
	fnTos(sn$="GLTB") : _
	lc=0 : mylen=25 : mypos=mylen+2
	fnChk(lc+=1,mypos,"List All Details",right) : _
	resp$(1)='True'
	fnLbl(lc+=1,1,"Cost Center:",mylen,right)
	fnTxt(lc,mypos,5,0,0,'number') : _
	resp$(2)=""
	fnChk(lc+=1,mypos,"Subtotal after each fund",right) : _
	resp$(3)='True'
	fnLbl(lc+=1,1,"Starting Account:",mylen,right)
	fnqgl(lc,mypos,0,1) : _
	resp$(4)="[All]"
	fnLbl(lc+=1,1,"Ending Account:",mylen,right)
	fnqgl(lc,mypos,0,1) : _
	resp$(5)="[All]"
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)='True' then pt=0 else pt=1
	costcent=val(resp$(2)) : _
	n$=lpad$(str$(costcent),3)&"     0  0"
	if resp$(3)='True' then subt=1 else subt=0
	sl1$=fnagl$(resp$(4)) : _
	sl2$=fnagl$(resp$(5))
START_REPORT: !
	fnopenprn
	gosub HDR2
READ_1: !
	read #1,using L410: n$,d$,bb,cb,mat bp eof L580
L410: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 171,13*pd 6.2
	olddno=dno
	if costcent><0 and val(n$(1:3))><costcent then goto READ_1
	if sgl(4)=0 and sgl(5)=0 and sgl(6)=0 then goto L460
	if n$>sl2$ then goto L590
L460: dno=val(n$(1:3))
	if subt=1 and olddno>0 and olddno<>dno then pr #255,using L480: "FUND "&str$(olddno)&" TOTALS",fundt3 else goto L500
L480: form skip 1,pos 30,c 20,pos 80,pic(zz,zzz,zzz.## cr),skip 2
	fundt1=fundt2=fundt3=0
L500: ano=val(n$(4:9))
	sno=val(n$(10:12))
	begbal=0
	curbal=curbal+bp(12)
	fundt1=fundt1+0
	fundt3=fundt3+bp(12)
	gosub L840
	goto READ_1
L580: olddno=dno
L590: pr #255:
	if subt=1 then : _
		pr #255,using L480: "Fund "&str$(olddno)&" Totals",0 ,0,fundt3
	pr #255: ,"Trial Balance Proof Totals";
	pr #255,using L630: 0,0,curbal
L630: form pos 80,pic(zz,zzz,zzz.## cr)
	close #1: ioerr L650
L650: close #2: ioerr L660
L660: fncloseprn
 
	goto Xit
 
HDR2: !
	pr #255,using L730: date$('mm/dd/yy'),cnam$
	pr #255,using L730: time$,cap$
L730: form pos 1,c 8,pos 15,cc 50
	pr #255,using L750: fnpedat$,"Page ",p1+=1
L750: form pos 15,cc 50,pos 80,c 5,n 4,skip 2
	pr #255,using L770: "Account","Reference","Ending"
L770: form pos 6,c 7,pos 70,c 9,pos 84,c 9,pos 99,c 7,pos 116,c 6,skip 1
	pr #255,using L790: "Number","Account Name/Transaction Description","Date  Source","Number","Balance"
L790: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7
	pr #255,using L810: "__________","____________________________________","____","______","___________","_________"
L810: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 10,skip 2
return
 
L840: !
	pr #255,using L860: dno,ano,sno,d$,bp(12) pageoflow L890
L860: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 50,pos 80,pic(nz,zzz,zzz.## cr),skip 2
return
 
L890: pr #255: newpage
	gosub HDR2
	continue
 
Xit: fnXit
 
include: ertn
 
