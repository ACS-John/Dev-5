! Replace S:\acsCL\TrJr
! pr Transaction Journals

	autoLibrary
	on error goto Ertn

	dim vnam$*30,de$*35,slt(3),ti$(3)*20,tr5$*30,item2$(2)*15
	dim t1(3),glt(3),glts(3),bn$*30
	dim des$*30,sltyn$(3)*1

	fnTop(program$, "Transaction Journals")
	ti$(1)="Checks"
	ti$(2)="Deposits"
	ti$(3)="Adjustments"

	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input
	read #20,using 'form POS 152,N 2': wbc
	close #20:

MAIN: !
	fnTos
	respc=0
	fnLbl(1,1,"Beginning Date:",38,1)
	fnTxt(1,40,10,0,1,"3",0,"Earliest transation date to be shown on journals!")
	resp$(respc+=1)=""
	fnLbl(2,1,"Ending Date:",38,1)
	fnTxt(2,40,10,0,1,"3",0,"Last transation date to be shown on journals!")
	resp$(respc+=1)=""
	fnLbl(4,1,"Information to Print:",38,1)
	item2$(1)="Details"
	item2$(2)="Totals Only"
	fncomboa("claims-act",4,40,mat item2$)
	resp$(respc+=1)=item2$(1)
	fnChk(7,40,"Print Disbursments Journal:",1)
	resp$(respc+=1)="True"
	fnChk(8,40,"Print Receipts Journal:",1)
	resp$(respc+=1)="True"
	fnChk(9,40,"Print Adjustments Journal:",1)
	resp$(respc+=1)="False"
	fnLbl(11,1,"Bank Account:",38,1)
	fncombof("Bankmstr",11,40,20,"[Q]\CLmstr\bankmstr.h[cno]",1,2,3,15,"[Q]\CLmstr\Bankidx1.h[cno]",1,0, "Select bank account for printing") : _
	resp$(respc+=1)=str$(wbc)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dt1=val(resp$(1)) ! beginning date
	dt2=val(resp$(2)) ! ending date
	td1yn$=resp$(3)(1:1) !  detail
	if resp$(4)(1:1)="T" then sltyn$(1)="Y" : slt(1)=1 else sltyn$(1)="N" ! disb jrn
	if resp$(5)(1:1)="T" then sltyn$(2)="Y" : slt(2)=1 else sltyn$(2)="N" ! rec jrn
	if resp$(6)(1:1)="T" then sltyn$(3)="Y" : slt(3)=1 else sltyn$(3)="N" ! adj jrn
	wbc=val(resp$(7)(1:2))
! FNWAIT
	open #trmstr=1: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,input,keyed
	open #tralloc=2: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,input,keyed
	open #glmstr=4: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,input,keyed
	open #work=3: "Name="&env$('temp')&'\'&"WORK,KFName="&env$('temp')&'\'&"ADDR,RecL=40,KPS=1,KLN=12,Replace",internal,outIn,keyed  : _
	! this file is used to total Amounts by General Ledger Number
	open #bankmstr=12: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed
	read #bankmstr,using 'Form POS 3,C 30,C 12,PD 6.2',key=cnvrt$("N 2",wbc),release: bn$ nokey MAIN
	close #bankmstr:
	bn$=rtrm$(bn$)
	fnopenprn
END1: !
	if wcd=0 or td1yn$="T" then goto HERE
	pr #255,using 'Form POS 52,G 12.2': "  __________"
	pr #255,using 'Form POS 52,G 12.2': t1(wcd)
	npg=1
HERE: if wcd>2 then goto ENDALL
	wcd+=1
	if slt(wcd)<>1 then goto HERE
	if npg=1 then : _
		pr #255: newpage
	npg=0
	if td1yn$="D" then gosub HDR
	restore #trmstr,key>=lpad$(str$(wbc),2)&str$(wcd)&"        ": nokey END1
READ_TRMSTR: !
	read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35': bank_code,tcde,checkNumber$,tr2,amt,vn$,de$ eof ENDALL
	if tcde=1 then de$=rpad$(ltrm$(vn$),8)&" "&de$(1:26)
	if bank_code><wbc then goto ENDALL
	if tcde><wcd then goto END1
	if fndate_mmddyy_to_ccyymmdd(tr2)<dt1 or fndate_mmddyy_to_ccyymmdd(tr2)>dt2 then : _
		goto READ_TRMSTR
	sq$=" "
	if tcde><1 then goto L750
	ck1=val(checkNumber$) conv L750
	if ck2=0 then goto L740
	if ck2+1><ck1 then sq$="*"
L740: ck2=ck1
L750: if td1yn$="D" then : _
		pr #255,using 'Form POS 1,C 2,C 10,PIC(ZZ/ZZ/ZZBB),C 29,N 12.2': sq$,checkNumber$,tr2,de$(1:29),amt pageoflow NEWPGE
	t1(wcd)+=amt
RESTORE_TRALLOC: !
	totalalloc=0 ! kj 52307
	key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&checkNumber$
	restore #tralloc,key>=key$: nokey PRINT_D_NEWPAGE
READ_TRALLOC: !
	read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,am2,tr5$,ivd$ eof PRINT_D_NEWPAGE : _
	if newkey$<>key$ then goto PRINT_D_NEWPAGE else : _
		if am2=0 then goto READ_TRALLOC
	totalalloc+=am2 ! kj 52307
	if wcd=2 then am2=-am2
	if td1yn$="D" then : _
		pr #255,using 'Form POS 66,C 12,N 11.2,X 2,C 32,c 6': gl$,am2,tr5$,ivd$ pageoflow NEWPGE
	goto SUMMARY_MAYBE

PRINT_D_NEWPAGE: !
	if amt<>totalalloc then pr #255,using "form pos 1,c 80": "The allocations on the above transaction do not agree with total transaction" ! kj 52307
	if td1yn$="D" then : _
		pr #255: pageoflow NEWPGE : _
		! if condition was left off and printed many blank pages if chose : _
		! totals only 4/04/01
	goto READ_TRMSTR

NEWPGE: pr #255: newpage: gosub HDR : continue

HDR: !
	pr #255,using 'Form POS 1,C 8,Cc 74': date$,env$('cnam')
	if end3=0 then : _
		pr #255,using 'Form POS 1,C 8,POS 24,CC 40': time$,"Bank # "&str$(wbc)&" "&bn$ : _
		pr #255,using 'Form POS 24,Cc 40': ti$(wcd)&" Journal"
	if end3=1 then : _
		pr #255,using 'Form POS 1,C 8,POS 24,CC 40': time$,"Bank # "&str$(wbc)&" "&bn$ : _
		pr #255,using 'Form POS 24,Cc 40': " General Ledger Recap"
	pr #255,using 'Form POS 1,C 26,C 60': "Page "&str$(pg+=1),"Date From: "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dt1)&" Date To: "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dt2)
	if end3=1 then goto EOHDR
	pr #255: "                                                                                  Item                                    Invoice "
	if wcd=1 then ref$="   Check # " else ref$="   Ref #   "
	pr #255: ref$& "   Date    Payee/Description                  Amount   GL Number      Amount   Item Description                  Date  "
	pr #255: "  ________  ________  _______________________________ __________ ____________ __________  _______________________________ ________"
EOHDR: return

ENDALL: if slt(wcd)=0 then goto L1080
	if td1yn$="D" then : _
		pr #255,using 'Form POS 52,G 12.2': "  __________"
	if td1yn$="D" then : _
		pr #255,using 'Form POS 52,G 12.2': t1(wcd)
	if td1yn$="T" then end3=1 : gosub HDR
	if td1yn$="T" then : _
		pr #255: "" : _
		pr #255,using 'Form POS 1,C 60': "______________________________________" : _
		pr #255: ""
L1080: for j=1 to 3 : _
		pr #255,using 'Form POS 7,C 18,N 12.2': "Total "&ti$(j),t1(j) : _
	next j
	if td1yn$="T" then : _
		pr #255: "" : _
		pr #255,using 'Form POS 1,C 60': "______________________________________" : _
		pr #255: ""
	gosub RESTORE_WORK
	fncloseprn
	goto Xit

Xit: fnXit

SUMMARY_MAYBE: !
	mat glt=(0)
	read #work,using 'Form POS 1,C 12,3*PD 6.2',key=gl$: gl$,mat glt nokey READ_WORK_NOKEY
	glt(wcd)+=am2
	rewrite #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt
	goto READ_TRALLOC ! was restore_tralloc
READ_WORK_NOKEY: glt(wcd)=am2
	write #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt
	goto READ_TRALLOC ! was RESTORE_TRALLOC (2)
RESTORE_WORK: !
	restore #work,key>="            ": nokey END3
	if td1yn$<>"T" then : _
		pr #255: newpage : _
		end3=1 : gosub HDR
	pr #255: "  GL Number   Disbursments    Receipts     Adjustments"
	pr #255: "____________  ____________  ____________  ____________"
READ_WORK: !
	read #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt eof END3
	if gl$(1:3)=hgl$(1:3) or val(hgl$)=0 then goto L1350
L1330: pr #255,using 'Form POS 1,C 60': "____________  ____________  ____________  ____________" : _
	pr #255,using 'Form POS 13,3*N 14.2': mat glts : _
	pr #255: ""
	mat glts=(0)
L1350: hgl$=gl$ : mat glts=glts+glt
	if subcode=1 then goto END3B
	des$=""
	read #glmstr,using 'Form POS 13,C 30',key=gl$: des$ nokey L1390
L1390: pr #255,using 'Form POS 1,C 12,3*N 14.2,X 2,C 30': gl$,mat glt,des$
	goto READ_WORK
! ___________________________
END3: !
	if val(hgl$(1:3))<>0 then subcode=1: goto L1330
END3B: !
	pr #255: "____________  ____________  ____________  ____________"
	pr #255,using 'Form POS 1,C 12,3*N 14.2,X 2,C 30': "   Totals",mat t1
return

include: Ertn
