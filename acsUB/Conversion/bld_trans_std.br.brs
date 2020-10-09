! Replace S:\acsUB\Conversion\Bld_Trans
! Builds an ubTransVB from ubTrans.h, UBmstr.h and ubAccTrn.h 
	! this conversion must be done before ubmaster is converted to customer
! this program assumes the following
	! service 1 is Water
	! service 2 is Sewer
	! service 3 is Electric
	! Service 4 is Gas
	! Service 5 is Sanitation
	! Service 6 is Fire Protection
	! Service 7 is Merchandise
	! Service 8 is Other
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,resp$(10)*80,g(11),acctrn_form$*80,rw4(22,13),key$*19,ru(6)
 
	fnTop("S:\acsUB\Conversion\Bld_Trans",cap$="Build Transactions")
	fncno(cno)
	pr newpage
LOOP_STEP_1: !
	gosub MENU1
	gosub CONVERT_CNO 
	goto Xit 
	! If CNO<>0 Then Gosub CONVERT_CNO : Goto LOOP_STEP_1 Else goto Xit
 
include: ertn
 
Xit: chain "S:\acsUB\conversion\UBmstr-vb"
 
MENU1: !
	fnTos
	fnLbl(1,1,"Convert Transactions")
	fnChk(4,1,"Delete existing transaction file before conversion")
	resp$(1)="True"
	fnChk(5,1,"Remove Transactions with Bad Dates")
	resp$(2)="False"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	delubtransvb$=resp$(1)
	removebaddates$=resp$(2)
	if ckey=5 then cno=0
 
return
 
CONVERT_CNO: !
	pr "conversion of cno=[cno] has begun."
 
 
	if uprc$(delubtransvb$)=uprc$("True") and exists("[Q]\UBmstr\ubtransvb.h[cno]") then execute "Free [Q]\UBmstr\ubtransvb.h[cno]"
 
	open #master=3: "Name=[Q]\UBmstr\ubMaster.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
 
! open NEW files
	open #transvb=11: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr,RecL=102,KPs=1,KLn=19,Use",internal,outIn,keyed
PHASE1: !
	pr 'moving trans from ubAccTrn to ubTranVB'
	open #acctrn=1: "Name=[Q]\UBmstr\ubAccTrn.h[cno],KFName=[Q]\UBmstr\ubAcTIx1.h[cno],Shr",internal,outIn,keyed
	if rln(acctrn)=64 or rln(acctrn)=72 then : _
		acctrn_form$='Form Pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2' : _
	else : _
		if rln(acctrn)=62 or rln(acctrn)=70 then : _
			acctrn_form$='Form Pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2' else : _
			if rln(acctrn)=68 then : _
				acctrn_form$='Form Pos 1,C 10,pd 4.2,n 8,n 1,n 1,10*pd 4.2'
READ_ACCTRN: !
L530: read #acctrn,using acctrn_form$: p$,tamt,tdate,transcode,postcode,g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10) eof PHASE2 ioerr R68F
	pr f "1,1,C 20,R,N": str$(accnt+=1)&"/"&str$(lrec(acctrn))
	read #master,using 'form pos 1,c 10',key=p$: z$ nokey READ_ACCTRN
	gosub TRANSLATE_TRANSCODE
	if len(str$(tdate))<=6 then goto L530
	postcode=9
	write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,d1,d3,d5,d7,d9,d11,bal,postcode
	goto READ_ACCTRN
PHASE2: !
	close #acctrn:
 
 
	pr 'moving trans from ubTrans to ubTranVB'
	open #trans=2: "Name=[Q]\UBmstr\ubTrans.h[cno]",internal,input
READ_TRANS: !
L680: read #trans,using 'Form Pos 1,C 10,pd 4.2,pd 4,n 1,n 1,pd 3': p$,tamt,tdate,transcode,postcode,nta eof PHASE3
	if postcode=5 then goto L680 ! don't get trans from current file that also been transferred to history
! If TDATE=101504 AND TRANSCODE=1 Then Goto 680 ! temporary !!!! skip october 15, 2004 charges
	read #master,using 'form pos 1,c 10,pos 300,11*pd 4.2',key=p$: z$,mat g nokey READ_TRANS
	if transcode<>1 then mat g=(0) ! only pull mat g from main record if charge transaction in current file
	gosub TRANSLATE_TRANSCODE
	if len(str$(tdate))<=6 then tdate=fndate_mmddyy_to_ccyymmdd(tdate)
	write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,d1,d3,d5,d7,d9,d11,bal,postcode
	goto READ_TRANS
PHASE3: !
	close #trans:
 
 
	pr 'moving trans from ubMaster to ubTranVB'
	restore #master:
READ_MASTER: !
	read #master,using 'form pos 1,c 10,pos 438,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2': p$,mat rw4 eof PHASE4
	for month=1 to 13
		tdate=fndate_mmddyy_to_ccyymmdd(rw4(8,month))
		if tdate=0 or tdate=20000000 then goto NEXT_MONTH
		g(01)=rw4(09,month) : g(02)=rw4(10,month) : _
		g(03)=rw4(11,month) : g(04)=rw4(12,month) : _
		g(05)=rw4(13,month) : g(06)=rw4(14,month) : _
		g(07)=rw4(15,month) : g(08)=rw4(16,month) : _
		g(09)=rw4(17,month) : g(10)=rw4(18,month) : _
		g(11)=rw4(19,month)
		ru(1)=rw4(1,month) : ru(2)=rw4(2,month) : _
		ru(3)=rw4(3,month) : ru(4)=rw4(4,month) : _
		ru(5)=rw4(5,month) : ru(6)=rw4(6,month) : _
		bal=rw4(7,month) : postcode=9 : _
		transcode=1 : tamt=rw4(19,month)
		key$=p$&lpad$(str$(tdate),8)&str$(transcode) : _
		read #transvb,using 'form pos 1,C 10',key=key$: p$ nokey WRITE_A_RECORD
		rewrite #transvb,using 'form pos 24,11*pd 4.2,6*pd 5,pd 4.2,n 1',key=key$: mat g,mat ru,bal,postcode
		goto NEXT_MONTH
WRITE_A_RECORD: !
		write #transvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,mat ru,bal,postcode
NEXT_MONTH: next month
	goto READ_MASTER
PHASE4: !
	close #master:
 
 
	close #transvb:
	pr "ReIndexing ubTransVB..."
	fnIndex("[Q]\UBmstr\UBTransVB.h[cno]","[Q]\UBmstr\UBTrIndx.h[cno]","1 19")
	pr "Transactions for company [cno] were built successfully."
	pr ""
	if removebaddates$="True" then gosub REMOVEBADDATES
return
 
TRANSLATE_TRANSCODE: !
	if transcode=1 and postcode=4 then transcode=5 : goto EOTT : _
		! Debit Memo
	if transcode=1 and postcode<>4 then transcode=1 : goto EOTT ! charge
	if transcode=2 then transcode=2 : goto EOTT ! penalty
	if transcode=3 then transcode=3 : goto EOTT ! collection
	if transcode=4 then transcode=4 : goto EOTT ! Credit Memo
EOTT: return  ! end of translate transcode
 
R68F: !
	if rln(acctrn)<>68 then goto ERTN
	if r68f=0 then : _
		acctrn_form$='Form Pos 1,C 10,pd 4.2,n 8,n 1,n 1,10*pd 4.2' : _
	else : _
		acctrn_form$='Form Pos 1,C 10,pd 4.2,x 2,n 6,n 1,n 1,10*pd 4.2'
	if r68f=1 then r68f=0 else r68f=1
	continue
 
REMOVEBADDATES: !
	open #transvb=11: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr,RecL=102,KPs=1,KLn=19,Use",internal,outIn,keyed
L1240: read #transvb,using "Form Pos 11,N 8": tdate eof L1270
	tdate$=str$(tdate) : _
	if val(tdate$(1:4))<1950 or val(tdate$(1:4))>2049 or val(tdate$(5:6))<1 or val(tdate$(5:6))>12 or val(tdate$(7:8))<1 or val(tdate$(7:8))>31 then : _
		delete #transvb:
	goto L1240
L1270: close #transvb:
return
