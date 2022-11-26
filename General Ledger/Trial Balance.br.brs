! formerly S:\acsGL\acglTB


autoLibrary
on error goto Ertn

fnTop(program$)
open #hCompany=fnH: "Name=[Q]\GLmstr\Company.h[cno]",i,i,r
dim cogl$(2)*12
read #hCompany,using 'form pos 152,2*C 12',rec=1: mat cogl$
close #hCompany:

open #hAcct=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",i,i,k ! formerly #1
open #hTran=fnH: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",i,i,r

if fnprocess=1 then goto DoReport
goto Screen1

Screen1: ! r:
	dim resp$(10)*80
	fnTos
	lc=rc=0 : mylen=25 : mypos=mylen+2
	! fnChk(lc+=1,mypos,"List All Details",1)
	! resp$(1)='True'
	fnLbl(lc+=1,1,"Cost Center:",mylen,1)
	fnTxt(lc,mypos,5,0,0,'number')
	resp$(rc_costCenter=rc+=1)=""
	lc+=1
	fnChk(lc+=1,mypos,"Subtotal after each fund",1)
	resp$(rc_subtotalFund=rc+=1)='True'
	lc+=1
	fnLbl(lc+=1,1,"Starting Account:",mylen,1)
	fnQgl(lc,mypos,0,1)
	resp$(rc_acctStart=rc+=1)="[All]"
	fnLbl(lc+=1,1,"Ending Account:",mylen,1)
	fnQgl(lc,mypos,0,1)
	resp$(rc_acctEnd=rc+=1)="[All]"
	! fnCmdSet(3)
		fnCmdKey("&Print with Details",1,1)
		fnCmdKey("&Print without Details",2,0)
		if env$('acsDeveloper')<>'' then
			fnCmdKey("&Export with Details",3,0)
		end if
		fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey=1 then enableDetails=1
	if ckey=2 then enableDetails=0
	if ckey=3 then enableExport=1 : enableDetails=1 else enableExport=0
	! pr 'enableDetails=';enableDetails : pause
	costcent=val(resp$(rc_costCenter))
	dim n$*12
	n$=lpad$(str$(costcent),3)&"     0  0"
	if resp$(rc_subtotalFund)='True' then subt=1 else subt=0
	sl1$=fnagl$(resp$(rc_acctStart))
	sl2$=fnagl$(resp$(rc_acctEnd))
	restore #hAcct,key>=n$: nokey Screen1

	restore #hAcct,key>=sl1$: nokey ignore

goto DoReport ! /r

SAVE_AS_OPEN_ERR: !
pr err
pause
retry


DoReport: ! r:
! on fkey 5 goto Finis
fnopenprn
if enableExport then 
		open #h_tmp=fnH: "Name=SAVE:"&fnsave_as_path$&"\*.csv,RecL=1,replace",external,output ioerr SAVE_AS_OPEN_ERR
		dim exportFile$*256
		exportFile$=os_filename$(file$(h_tmp))
		close #h_tmp,free:
		dim serverTempSaveFile$*256
		serverTempSaveFile$='[temp]\save_[session].csv'
		open #hExp=fnH: 'name='&serverTempSaveFile$&',recl=1024,replace',d,o
		delim$=chr$(9)
end if
gosub PrHdr
do
	ReadAcct: !
	dim d$*50
	dim ta(2)
	read #hAcct,using Facct: n$,d$,bb,cb,mat ta eof EoAcct
	Facct: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 333,2*pd 3
	olddno=dno
	if costcent=0 or val(n$(1:3))=costcent then
		dno=val(n$(1:3))
		if subt=1 and olddno>0 and olddno<>dno then
			pr #255,using FprFundTotal: "FUND "&str$(olddno)&" TOTALS",fundT1,fundT2,fundT3
			FprFundTotal: form skip 1,pos 30,c 20,pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(zz,zzz,zzz.## cr),skip 2
			fundT1=fundT2=fundT3=0
		end if
		ano=val(n$(4:9))
		sno=val(n$(10:12))
		begbal+=bb
		curbal+=cb
		fundT1+=bb
		fundT3+=cb
		gosub PrLine
		if ta(1) then
			adr=ta(1)
			do until adr=0
				dim t$*12
				dim tr$*12
				dim tr(7)
				dim td$*30
				read #hTran,using Ftran,rec=adr: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,adr
				Ftran: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
				gosub AccumAndPrint
			loop
			gosub PrSummary
		end if
	end if
loop
EoAcct: !
olddno=dno
goto Finis ! /r
Finis: ! r:
	pr #255:
	if subt=1 then
		pr #255,using FprFundTotal: "Fund "&str$(olddno)&" Totals",fundT1,fundT2,fundT3
	end if
	pr #255: ,"Trial Balance Proof Totals";
	pr #255,using L990: begbal,trtotal,curbal
	L990: form pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(zz,zzz,zzz.## cr)
	close #hAcct: ioerr ignore
	close #hTran: ioerr ignore
	fncloseprn
	if enableExport then
		fnMakeSurePathExists(env$('at')&exportFile$)
		close #hExp:
		fnCopyFile(serverTempSaveFile$,env$('at')&exportFile$)
		exe 'Sy -@ -m -C start "Trial Balance Export" "C:\Program Files\Microsoft Office\root\Office16\excel.exe""'&exportFile$&'"'
	end if

goto Xit ! /r

PrHdr: ! r:
	pr #255,using Fh1: date$('mm/dd/yy'),env$('cnam')
	pr #255,using Fh1: time$,env$('program_caption')
	Fh1: form pos 1,c 8,pos 15,cc 50
	pr #255,using Fh2: fnpedat$,"Page ",p1+=1
	Fh2: form pos 15,cc 50,pos 115,c 5,n 4,skip 2
	pr #255,using Fh3: "Account","Reference","Beginning","Current","Ending"
	Fh3: form pos 6,c 7,pos 70,c 9,pos 84,c 9,pos 99,c 7,pos 116,c 6,skip 1
	pr #255,using Fh4: "Number","Account Name/Transaction Description","Date  Source","Number","Balance","Activity","Balance"
	Fh4: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7,pos 99,c 8,pos 116,c 7
	pr #255,using Fh5: "__________","____________________________________","____","______","___________","_________","__________","_________"
	Fh5: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 9,pos 98,c 10,pos 115,c 10,skip 2
	if enableExport then
		pr #hExp: 'Acct Id'&delim$&'Acct Name or Trans Desc'&delim$&'Date'&delim$&'Source'&delim$&'Reference Number'&delim$&'Beginning Balance'&delim$&'Current Activity'&delim$&'Ending Balance'
	end if
return ! /r
PrLine: ! r:
	if ta(1)=0 then
		if enableDetails then
			pr #255,using Fl1: dno,ano,sno,d$,bb,cb pageoflow PgOf
			Fl1: form pos 1,pic(---),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
			if enableExport then 
				pr #hExp,using Fl1: dno,ano,sno,d$,bb,cb pageoflow ExpPgOf
			end if
		end if
	else
		if enableDetails then
			pr #255,using Fl2: dno,ano,sno,d$,bb pageoflow PgOf
			Fl2: form pos 1,pic(---),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(zz,zzz,zzz.## cr)
			if enableExport then
				pr #hExp,using Fl2: dno,ano,sno,d$,bb pageoflow ExpPgOf
			end if
		end if
	end if
return ! /r
AccumAndPrint: ! r:
	dim a$(9)*3
	a$(1)="C/D"
	a$(2)="C/R"
	a$(3)="ADJ"
	a$(4)="A/P"
	a$(5)="PR"
	a$(6)="A/R"
	a$(7)="S/J"
	a$(8)="P/J"
	a$(9)=" "
	dim x$*3
	if tr(6)<1 or tr(6)>9 then x$="" else x$=a$(tr(6))
	if val(cogl$(1)(4:9))=0 or val(cogl$(2)(4:9))=0 then
		goto AapNext
	end if
	if t$>=cogl$(1) and t$<=cogl$(2) then
		if tr(5)>0 then goto AapNext
		u0+=tr(5)
		trtotal+=tr(5)
		fundT2+=tr(5)
		dim u$*12
		u$=t$
		goto AapXit
	end if
	AapNext: !

	if tr$="999999999999" then tr$=" "
	rn=73-int(len(ltrm$(tr$))/2)
	if tr(5)<0 then tcr1+=tr(5) else tdr1+=tr(5)
	if adr=0 and u0=0 then
		if enableDetails then
			pr #255,using Faap1: td$,tr(4),x$,ltrm$(tr$),tr(5),cb pageoflow PgOf
			Faap1: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos rn,c 12,pos 95,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
			if enableExport then
				pr #hExp,using Faap1: td$,tr(4),x$,ltrm$(tr$),tr(5),cb pageoflow ExpPgOf
			end if
		end if
		! pr #255,Using 'form pos 40,2*c 35,skip 2': "Total Debits: "&LTRM$(CNVRT$("PIC(ZZZZ,ZZZ,ZZZ,ZZ#.##)",TDR1)),"Total Credits: "&LTRM$(CNVRT$("PIC(ZZZZ,ZZZ,ZZZ,ZZ#.##)",ABS(TCR1)))
		tdr1=tcr1=0
	else
		if enableDetails then
			pr #255,using Faap2: td$,tr(4),x$,ltrm$(tr$),tr(5) pageoflow PgOf
			Faap2: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos rn,c 12,pos 95,pic(zz,zzz,zzz.## cr)
			if enableExport then
				pr #hExp,using Faap2: td$,tr(4),x$,ltrm$(tr$),tr(5) pageoflow ExpPgOf
			end if
		end if
	end if
	trtotal+=tr(5)
	fundT2+=tr(5)
	u$=t$
	AapXit: !
return ! /r
PrSummary: ! r:
	if u0 then
		if u$>=cogl$(1) and u$<=cogl$(2) then
			if enableDetails then
				pr #255,using L1550: "Summary Transaction",u0,cb pageoflow PgOf
				L1550: form pos 21,c 30,pos 95,pic(zz,zzz,zzz.## cr),pos 111,pic(zz,zzz,zzz.## cr),skip 2
				if enableExport then
					pr #hExp,using L1550: "Summary Transaction",u0,cb pageoflow ExpPgOf
				end if
			end if
			u0=0
		end if
	end if
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHdr
continue ! /r
ExpPgOf: ! r:
	pr #hExp: newpage
continue ! /r
Xit: fnXit
include: ertn
