! formerly S:\acsGL\acglTB
! pr Trial Balance
 
	autoLibrary
	on error goto Ertn
 
	dim d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
	dim a$(9)*3,cogl$(2)*12,u$*12,d(2),ta(2)
	dim resp$(10)*80
 
	right=1
	fnTop(program$)
	open #20: "Name=[Q]\GLmstr\Company.h[cno]",internal,input,relative
	read #20,using 'Form POS 150,2*N 1',rec=1: d(1),d(2)
	read #20,using 'Form POS 152,2*C 12',rec=1: mat cogl$
	close #20:
	a$(1)="C/D" : a$(2)="C/R" : a$(3)="ADJ"
	a$(4)="A/P" : a$(5)="PR" : a$(6)="A/R"
	a$(7)="S/J" : a$(8)="P/J" : a$(9)=" "
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\GLmstr\GLTrans.H[cno],Shr",internal,input,relative
	if fnprocess=1 then goto START_REPORT
SCREEN1: !
	fnTos(sn$="GLTB")
	lc=0 : mylen=25 : mypos=mylen+2
	fnChk(lc+=1,mypos,"List All Details",right)
	resp$(1)="True"
	fnLbl(lc+=1,1,"Cost Center:",mylen,right)
	fnTxt(lc,mypos,5,0,0,'number')
	resp$(2)=""
	fnChk(lc+=1,mypos,"Subtotal after each fund",right)
	resp$(3)="True"
	fnLbl(lc+=1,1,"Starting Account:",mylen,right)
	fnqgl(lc,mypos,0,1)
	resp$(4)="[All]"
	fnLbl(lc+=1,1,"Ending Account:",mylen,right)
	fnqgl(lc,mypos,0,1)
	resp$(5)="[All]"
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)="True" then pt=0 else pt=1
	costcent=val(resp$(2))
	n$=lpad$(str$(costcent),3)&"     0  0"
	if resp$(3)="True" then subt=1 else subt=0
	sl1$=fnagl$(resp$(4))
	sl2$=fnagl$(resp$(5))
	restore #1,key>=n$: nokey SCREEN1
	if fnprocess=0 then
		restore #1,key>=sl1$: nokey ignore
	end if
START_REPORT: ! r:
	on fkey 5 goto L950
	fnopenprn
	gosub HDR2
! IF D(1)=0 OR fnPROCESS=1 THEN GOTO 310 ELSE GOTO 340
READ_1: !
	read #1,using L690: n$,d$,bb,cb,mat ta eof L940
	L690: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 333,2*pd 3
	olddno=dno
	if costcent><0 and val(n$(1:3))><costcent then goto READ_1
	if sgl(4)=0 and sgl(5)=0 and sgl(6)=0 then goto L740
	if n$>sl2$ then goto L950
	L740: dno=val(n$(1:3))
	if subt=1 and olddno>0 and olddno<>dno then pr #255,using L760: "FUND "&str$(olddno)&" TOTALS",fundt1,fundt2,fundt3 else goto L780
	L760: form skip 1,pos 30,c 20,pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(zz,zzz,zzz.## cr),skip 2
	fundt1=fundt2=fundt3=0
	L780: ano=val(n$(4:9))
	sno=val(n$(10:12))
	begbal=begbal+bb
	curbal=curbal+cb
	fundt1=fundt1+bb
	fundt3=fundt3+cb
	gosub L1200
	if ta(1)=0 then goto READ_1
	adr=ta(1)
	do until adr=0
		read #2,using L890,rec=adr: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,adr
		L890: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
		gosub L1290
	loop
	gosub L1520
	goto READ_1
L940: olddno=dno
L950: pr #255:
	if subt=1 then
		pr #255,using L760: "Fund "&str$(olddno)&" Totals",fundt1,fundt2,fundt3
	end if
	pr #255: ,"Trial Balance Proof Totals";
	pr #255,using L990: begbal,trtotal,curbal
	L990: form pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(zz,zzz,zzz.## cr)
	close #1: ioerr ignore
	close #2: ioerr ignore
	fncloseprn
 
	goto Xit ! /r
 
HDR2: ! r:
	pr #255,using L1090: date$('mm/dd/yy'),env$('cnam')
	pr #255,using L1090: time$,env$('program_caption')
	L1090: form pos 1,c 8,pos 15,cc 50
	pr #255,using L1110: fnpedat$,"Page ",p1+=1
	L1110: form pos 15,cc 50,pos 115,c 5,n 4,skip 2
	pr #255,using L1130: "Account","Reference","Beginning","Current","Ending"
	L1130: form pos 6,c 7,pos 70,c 9,pos 84,c 9,pos 99,c 7,pos 116,c 6,skip 1
	pr #255,using L1150: "Number","Account Name/Transaction Description","Date  Source","Number","Balance","Activity","Balance"
	L1150: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7,pos 99,c 8,pos 116,c 7
	pr #255,using L1170: "__________","____________________________________","____","______","___________","_________","__________","_________"
	L1170: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 9,pos 98,c 10,pos 115,c 10,skip 2
return ! /r
L1200: ! r:
if ta(1)=0 then
		if pt=0 then
			pr #255,using L1220: dno,ano,sno,d$,bb,cb pageoflow PGOF
			L1220: form pos 1,pic(---),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
		end if
	else
		if pt=0 then
			pr #255,using L1250: dno,ano,sno,d$,bb pageoflow PGOF
			L1250: form pos 1,pic(---),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(zz,zzz,zzz.## cr)
		end if
	end if
return ! /r
L1290: ! r:
	if tr(6)<1 or tr(6)>9 then x$="" else x$=a$(tr(6))
	if val(cogl$(1)(4:9))=0 or val(cogl$(2)(4:9))=0 then goto L1360
	if t$>=cogl$(1) and t$<=cogl$(2) then goto L1320 else goto L1360
	L1320: !
	if tr(5)>0 then goto L1360
	u0+=tr(5) : trtotal+=tr(5): fundt2+=tr(5) : u$=t$
	goto L1500
	L1360: !
	if tr$="999999999999" then tr$=" "
	rn=73-int(len(ltrm$(tr$))/2)
	if tr(5)<0 then tcr1+=tr(5) else tdr1+=tr(5)
	if adr=0 and u0=0 then goto L1400 else goto L1460
	L1400: !
	if pt=0 then
		pr #255,using L1410: td$,tr(4),x$,ltrm$(tr$),tr(5),cb pageoflow PGOF
	end if
	L1410: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos rn,c 12,pos 95,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
	! pr #255,Using 'form pos 40,2*c 35,skip 2': "Total Debits: "&LTRM$(CNVRT$("PIC(ZZZZ,ZZZ,ZZZ,ZZ#.##)",TDR1)),"Total Credits: "&LTRM$(CNVRT$("PIC(ZZZZ,ZZZ,ZZZ,ZZ#.##)",ABS(TCR1)))
	tdr1=tcr1=0
	goto L1480
	L1460: !
	if pt=0 then
		pr #255,using L1470: td$,tr(4),x$,ltrm$(tr$),tr(5) pageoflow PGOF
	end if
	L1470: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos rn,c 12,pos 95,pic(zz,zzz,zzz.## cr)
	L1480: !
	trtotal+=tr(5) : fundt2+=tr(5)
	u$=t$
	L1500: !
return ! /r
L1520: ! r:
	if u0=0 then goto L1570
	if u$<cogl$(1) or u$>cogl$(2) then goto L1570
	if pt=0 then pr #255,using L1550: "Summary Transaction",u0,cb pageoflow PGOF
	L1550: form pos 21,c 30,pos 95,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
	u0=0
	L1570:  !
return ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HDR2
continue ! /r
Xit: fnXit
include: ertn
