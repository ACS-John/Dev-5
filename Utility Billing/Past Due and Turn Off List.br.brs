! formerly S:\acsUB\ubPDTnOf
! r: initial stuff
	library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnerror,fnTos,fnopenprn,fncloseprn,fnxit,fncomboa,fnFra,fnLastBillingDate,fnCmdSet,fntop,fnChk,fndat,fncreg_read,fncreg_write,fnget_services,fngethandle
	on error goto Ertn
! 
	dim resp$(20)*80
	dim z$*10,e$*30,g(12),metradr$*30
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),month(4),dat$*20
! 
	fntop(program$)
	fnLastBillingDate(lbill)
	fndat(dat$)
	dim opt_aai$(3)
	opt_aai$(1)="[All]"
	opt_aai$(2)="Active"
	opt_aai$(3)="Inactive"
	fncreg_read('ubpdtnof.aai',aai$,opt_aai$(1))
	fncreg_read('ubpdtnof.printadr',printadr$,'False')
	fncreg_read('ubpdtnof.excludecurrent',excludecurrent$,'False')
	fncreg_read('ubpdtnof.excludelast',excludelast$,'False')
	fncreg_read('ubpdtnof.pastduebalance',pastduebalance$,'False')
	fncreg_read('ubpdtnof.pr_s4_meter_number',pr_s4_meter_number$,'False')
	fncreg_read('ubpdtnof.pr_blank_lines_for_notes',pr_blank_lines_for_notes$,'False')
	fncreg_read('ubpdtnof.accountSequence',accountSequence$,'True')

	dim srvnam$(10)*20,srv$(10)*2
	fnget_services(mat srvnam$,mat srv$)
! /r
! r: SCREEN1
	fnTos(sn$="UBPdTnOf")
	mylen=21 : mypos=mylen+2
	fnFra(1,1,3,40,"Aging Dates","Use the last day of each month for your aging dates (Use ccyymmdd format).")

	fnLbl(1,1,"Current Month:",mylen,1,0,1)
	fnTxt(1,mypos,10,10,1,"3",0,"Use the last day of your current mongh for the best aging results.",1)
	resp$(1)=""
	fnLbl(2,1,"Last Month:",mylen,1,0,1)
	fnTxt(2,mypos,10,10,1,"3",0,"Use the last day of last month.",1)
	resp$(2)=""
	fnLbl(3,1,"Month Before That:",mylen,1,0,1)
	fnTxt(3,mypos,10,10,1,"3",0,"Equivalent date from two months ago.",1)
	resp$(3)=""
	fnLbl(6,1,"Billing Date:",mylen,1)
	fnTxt(6,mypos,8,8,1,"1")
	resp$(4)=str$(lbill)
	fnLbl(7,1,"Final Billing Code:" ,mylen,1)
	fncomboa("aai",7,mypos,mat opt_aai$)
	resp$(5)=aai$
	fnChk(9,40,"Print Meter Address:",1)
	resp$(6)=printadr$
	fnChk(10,40,"Exclude Current Month:",1)
	resp$(7)=excludecurrent$
	fnChk(11,40,"Exclude Last Month:",1)
	resp$(8)=excludelast$
	fnChk(12,40,"Print Past Due Balance:",1)
	resp$(9)=pastduebalance$
	fnChk(13,40,"Print "&trim$(srvnam$(4))&" Meter Number:",1)
	resp$(10)=pr_s4_meter_number$
	fnChk(15,40,"Print blank lines for notes:",1)
	resp$(11)=pr_blank_lines_for_notes$
	fnChk(17,40,"Account Sequence",1)
	resp$(rc_accountSequence:=12)=accountSequence$
	fnCmdSet(3)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto XIT
	for j=1 to 3
L400: x=pos(resp$(j),"/",1)
		if x>0 then resp$(j)(x:x)="" : goto L400
	next j
	lastday(1)=val(resp$(1))
	firstday(1)=(val(resp$(1)(1:6))*100)+1
	lastday(2)= val(resp$(2))
	firstday(2)=(val(resp$(2)(1:6))*100)+1
	lastday(3)=val(resp$(3))
	firstday(3)=(val(resp$(3)(1:6))*100)+1
	lbill=val(resp$(4))
	aai$=printal$=resp$(5)
	printadr$=resp$(6) : if printadr$="True" then printadr=1 ! wants meter address printed
	excludecurrent$=resp$(7) : if excludecurrent$="True" then excludecurrent=1 ! do not list those owing just the current month
	excludelast$=resp$(8) : if excludelast$="True" then excludelast=1 ! do not list those owing just the current month and last month
	pastduebalance$=resp$(9) : if pastduebalance$="True" then pastduebalance=1 ! only show past due amount in balance column
	pr_s4_meter_number$=resp$(10) : if pr_s4_meter_number$="True" then pr_s4_meter_number=1 ! only show past due amount in balance column
	pr_blank_lines_for_notes$=resp$(11)
	accountSequence$=resp$(rc_accountSequence)
	fncreg_write('ubpdtnof.aai',aai$)
	fncreg_write('ubpdtnof.printadr',printadr$)
	fncreg_write('ubpdtnof.excludecurrent',excludecurrent$)
	fncreg_write('ubpdtnof.excludelast',excludelast$)
	fncreg_write('ubpdtnof.pastduebalance',pastduebalance$)
	fncreg_write('ubpdtnof.pr_s4_meter_number',pr_s4_meter_number$)
	fncreg_write('ubpdtnof.pr_blank_lines_for_notes',pr_blank_lines_for_notes$)
	fncreg_write('ubpdtnof.accountSequence',accountSequence$)
! /r
	on fkey 5 goto DONE
! r: the report
! r: report setup
	fnopenprn
	gosub HDR1
	if accountSequence$='True' then
		open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
	else
		open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed 
	end if
	open #hTrans:=fngethandle: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
	! open #ratemst:=8: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed 
	gosub BUD1
! /r
MAIN_LOOP_TOP: ! 
	read #hCustomer,using F_CUSTOMER: z$,metradr$,e$,a7,final,bal,f,mat g,s4_meter_number$,route eof TOTAL_FINAL
F_CUSTOMER: form pos 1,c 10,c 30,pos 41,c 30,pos 155,pd 2,pos 1821,n 1,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 373,c 12,pos 1741,n 2
	if bud1=1 then gosub BUD2
	if bd1>0 then goto L650 ! IF BUDGET BILLING AND HAVE NOT PAID LAST BILL, LIST ANYWAY   ( BD1=# OF BUDGET BILLS NOT PAID)
	if totba>0 and bd1=0 then goto MAIN_LOOP_TOP ! DON'T LIST IF BUDGET BILL AND HAVE PAID LAST BILL (NO MATTER WHAT BALANCE)
	if bal<=1 then goto MAIN_LOOP_TOP
	if final=3 then final=0 ! consider active customer who are not be billed the same as regular active customers.
	if printal$=opt_aai$(2) and final>0 then goto MAIN_LOOP_TOP
	if printal$=opt_aai$(3) and final=0 then goto MAIN_LOOP_TOP
L650: ! 
	if holdrt=0 or route=0 or holdrt=route then goto L690
	gosub TOTAL_BOOK
	pr #255: newpage
	gosub HDR1
L690: ! 
	gosub TRANS_ACCUMULATE 
! 
! if trim$(z$)='100780.00' then pause
	az$=""
	if month(4)>0 or bd1=3 then 
		az$="****"
	else if month(3)>0 or bd1=2 then 
		az$="***"
	else if month(2)>0 or bd1=1 then 
		az$="**"
	end if 
! 
	if totba>0 then lev$="L" else lev$=""
	if excludecurrent=1 and len(az$)<2 then goto L880 ! exclude those oweing only current month
	if excludelast=1 and len(az$)<3 then goto L880 ! exclude those oweing only current month  or previous month
	if pastduebalance=1 and excludecurrent=1 then bal=bal-month(1) ! don't show current in past due balance column
	if pastduebalance=1 and excludelast=1 then bal=bal-month(2) ! don't show last month in past due balance column
! 
	if printadr and pr_s4_meter_number then 
		pr #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$,metradr$(1:25),fn_s4_meter_number$ pageoflow PGOF
	else if pr_s4_meter_number then 
		pr #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$,fn_s4_meter_number$ pageoflow PGOF
	else if printadr then 
		pr #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$,metradr$(1:25) pageoflow PGOF
	else 
		pr #255,using F_REPORT_LINE: z$,e$(1:25),bal,f,az$,lev$ pageoflow PGOF
	end if 
	if trans_accumulate_execption$<>'' then 
		pr #255,using 'form pos 64,C 24': trans_accumulate_execption$ pageoflow PGOF
	end if 
	if pr_blank_lines_for_notes$='True' then 
		pr #255: '' pageoflow PGOF
		pr #255: rpt$('_',71) pageoflow PGOF
		pr #255: '' pageoflow PGOF
	end if 
F_REPORT_LINE: form pos 1,c 12,c 25,n 12.2,pic(bbzz/zz/zzbb),x 3,c 4,x 1,c 1,x 2,c 25,x 2,c 25
! 
	s2=s2+bal
	t2=t2+bal
	t1=t1+g(10)
	s1=s1+g(10)
	holdrt=route
L880: ! 
	goto MAIN_LOOP_TOP
! /r
HDR1: ! r:
	p2=p2+1
	pr #255: "\qc "&env$('cnam')
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
! pr #255: "As of "&CNVRT$("pic(zzzz/zz/zz)",LASTDAY(1))
	pr #255: "As of "&dat$
	pr #255,using L970: "\ql "&date$,"Page "&str$(p2)
L970: form pos 1,c 70,cr 14
	if pastduebalance=1 then 
		pr #255: "                                         Past Due  Last Bill   Turn"
	else 
		pr #255: "                                         Current   Last Bill   Turn"
	end if 
	if printadr and pr_s4_meter_number then 
		pr #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance } {\ul    Date   }  {\ul  Off}      {\ul Meter Address            }  {\ul Meter Number}"
	else if pr_s4_meter_number then 
		pr #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance } {\ul    Date   }  {\ul  Off}      {\ul Meter Number}"
	else if printadr then 
		pr #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance } {\ul    Date   }  {\ul  Off}      {\ul Meter Address            }"
	else 
		pr #255: "{\ul Account No}  {\ul Customer Name            }  {\ul   Balance }  {\ul    Date   }  {\ul  Off}"
	end if 
	return  ! /r
TOTAL_BOOK: ! r:
	pr #255: "" pageoflow PGOF
	pr #255: "" pageoflow PGOF
	pr #255: "Totals For Route Number ";holdrt;
	pr #255,using F_PR_TOTAL: s2 pageoflow PGOF
F_PR_TOTAL: form pos 38,pic(-,---,---.##)
	pr #255: "" pageoflow PGOF
	s1=0
	s2=0
	holdrt=route
	return  ! /r
TOTAL_FINAL: ! r:
	gosub TOTAL_BOOK
! TOTAL_GRAND: !
	s1=t1 : s2=t2
	pr #255: ""
	pr #255: ""
	pr #255: "               Grand Totals";
	pr #255,using F_PR_TOTAL: s2
	goto DONE ! /r
DONE: ! r:
	close #hCustomer: ioerr ignore
	close #hTrans: ioerr ignore
	fncloseprn
	goto XIT ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HDR1
	continue  ! /r
XIT: fnxit
IGNORE: continue 
BUD1: ! r:
	bud1=0
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L1390
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative 
	bud1=1
L1390: return  ! /r
BUD2: ! r:
	bd1=totba=0
	mat bd1(5) : mat bd1=(0) : mat bd2=(0)
	if bud1=0 then goto L1580
	read #81,using L1470,key=z$: z$,mat ba,mat badr nokey L1580
	for j=2 to 12: totba=totba+ba(j): next j
L1470: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	ta1=badr(1)
L1490: if ta1=0 then goto L1580
	read #82,using L1510,rec=ta1: z$,mat bt1,nba noRec L1580
L1510: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(14,1)>0 then goto L1570
	bd1=bd1+1
	if bd1>5 then goto L1580
	bd1(bd1)=bt1(1,2)
	bd2(bd1)=ta1
	L1570: ta1=nba : goto L1490
	L1580: ! 
return  ! /r
PR_TRAN_DEBUG_DATA: ! r:
	if debug_this_tran then pr '                            tdate=';tdate;' tcode=';tcode;' tamount=';tamount : debug_this_tran=1 else debug_this_tran=0 ! pause
	return  ! /r
TRANS_ACCUMULATE: ! r:
	mat month=(0)
	dim trans_accumulate_execption$*24
	trans_accumulate_execption$=''
! if trim$(z$)='100093.57' then pause
	restore #hTrans,key>=z$&"         ": nokey TA_NO_TRANS
	ta_p1_read_count=0
TA_TRANS_READ: ! 
	read #hTrans,using 'form pos 1,c 10,n 8,n 1,pd 4.2': p$,tdate,tcode,tamount eof TA_PHASE_2
	ta_p1_read_count+=1
! if env$('ACSDeveloper')<>'' and trim$(z$)='100780.00' then debug_this_account=1 else debug_this_account=0 ! pause
! if debug_this_account and str$(tdate)(1:4)='2015' then debug_this_tran=1 else debug_this_tran=0 ! pause
	if p$<>z$ and ta_p1_read_count=1 then trans_accumulate_execption$='(no transaction history)'
	if p$<>z$ then goto TA_PHASE_2
	for j=1 to 3
		if tdate<firstday(3) then ! older than we want to analyze
!     if debug_this_tran then pr '  A  older than what we want to analyze'
			goto TA_TRANS_READ
		else if tdate>=firstday(j) and tdate<=lastday(j) and (tcode = 1 or tcode=2 or tcode=5) then 
			month(j)=month(j)+tamount
!      if debug_this_tran then gosub PR_TRAN_DEBUG_DATA : pr '  B  month(';j;')=';month(j);'    because ';tdate;' =/between >';firstday(j);' - ';lastday(j);' and tcode=';tcode
			goto TA_TRANS_READ
		else if tdate>lastday(j) and (tcode = 1 or tcode=2 or tcode=5) then ! accumulate all collections in month 4
			month(j)=month(j)+tamount
!      if debug_this_tran then gosub PR_TRAN_DEBUG_DATA : pr '  C  month(';j;')=';month(j);'    because ';tdate;'>';lastday(j);' and tcode=';tcode
			goto TA_TRANS_READ
		end if 
	next j
	goto TA_TRANS_READ
TA_PHASE_2: ! 
	if debug_this_account then pr ' the month accumulators before pahse 2' : pr mat month ! pause
	holdbal=bal
	for j=1 to 4 ! find oldest month still owed
		if holdbal<=0 then 
!     if debug_this_account then pr '  AA  changing month(';j;') from ';month(j);' to 0 because holdbal<=0'
			month(j)=0
!   else if env$('client')="Albany" and holdbal<10.50 then ! don't any balance less than a minimum bill cause a month to show delinquent
!     month(j)=0
		else if holdbal>0 then 
!     if debug_this_account then pr '  CC  changing holdbal from (';holdbal;') to ';holdbal-month(j)
			holdbal=holdbal-month(j)
		end if 
!   if debug_this_account then pause
	next j
	goto TA_FINIS
TA_NO_TRANS: ! 
	trans_accumulate_execption$='(no transaction history)'
	goto TA_PHASE_2 ! TA_FINIS
TA_FINIS: ! 
	if debug_this_account then 
		pr ' the month accumulators AFTER pahse 2' : pr mat month
		pr ' trans_accumulate_execption$='&trans_accumulate_execption$
		pause 
	end if 
return  ! /r
def fn_s4_meter_number$*12
	fn_s4_meter_number$=s4_meter_number$
fnend 
include: ertn
