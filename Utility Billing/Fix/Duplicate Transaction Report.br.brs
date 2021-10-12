autoLibrary
on error goto Ertn

dim z$*10,tg(11),resp$(10)*80
fnTop(program$,"Duplicate Transaction Report")

open #fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed
open #fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed
open #h_trans1=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",i,i,r
open #h_trans2=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",i,i,k
trans1_lrec=lrec(h_trans1)
Ftrans: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1

del_dupe=1

fnTos : respc=lc=0
fn_filter_add_chk('Account','True')
fn_filter_add_chk('Transaction Date','False')
fn_filter_add_chk('Amount','True')
fn_filter_add_chk('Transaction Type','True')
lc+=1
fnLbl(lc+=1,1,"Starting Record:",16,1)
fnTxt(lc,18,10,0,0,'30')
resp$(respc+=1)=str$(max(1,trans1_lrec-1000))
fnLbl(lc+=1,1,"Ending Record:",16,1)
fnTxt(lc,18,10,0,0,'30')
resp$(respc+=1)=str$(trans1_lrec)
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
respc=0
dupe(1)=fn_filter_get_chk('Account',resp$(respc+=1))
dupe(2)=fn_filter_get_chk('Transaction Date',resp$(respc+=1))
dupe(3)=fn_filter_get_chk('Amount',resp$(respc+=1))
dupe(4)=fn_filter_get_chk('Transaction Type',resp$(respc+=1))
rec_start=val(resp$(respc+=1))
rec_end=val(resp$(respc+=1))
fnopenprn
fn_header
! restore #h_trans1,rec=rec_start: noRec NEXT_REC
trans1_rec=rec_start-1
do
	NEXT_REC: !
	trans1_rec+=1
	if trans1_rec>trans1_lrec or (rec_end>0 and rec_end<trans1_rec) then goto FINIS
	read #h_trans1,using Ftrans,rec=trans1_rec: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode noRec NEXT_REC
	pr trans1_rec
	if fn_has_dupe then
		pr #255,using FORM_OUT: trans1_rec,p$,tdate,tamount pageoflow PgOf
		FORM_OUT: form n 8,x 1,c 10,x 1,x 1,pic(zzzz/zz/zz),n 11.2
	end if  ! fn_has_dupe(z$)
loop

FINIS: !
	fncloseprn
	close #h_trans1:
Xit: !
fnXit
 
PgOf: ! r:
	pr #255: newpage
	fn_header
continue ! /r
def fn_has_dupe
	hd_return=0
	dim hd_tg(11)
	if del_dupe then
		restore #h_trans2:
		do
			read #h_trans2,using Ftrans: hd_p$,hd_tdate,hd_tcode,hd_tamount,mat hd_tg,hd_wr,hd_wu,hd_er,hd_eu,hd_gr,hd_gu,hd_tbal,hd_pcode eof HD_EOF
			if ~dupe(1) or p$=hd_p$ then
				if ~dupe(2) or tdate=hd_tdate then
					if ~dupe(3) or tamount=hd_tamount then
					  if ~dupe(4) or tcode=hd_tcode then
					    if trans1_rec<>rec(h_trans2) then
					      hd_return=1
					      fn_trans_delete(rec(h_trans2))
					    end if
					  end if
					end if
				end if
			end if
		loop
	else
		restore #h_trans2:
		do
			read #h_trans2,using Ftrans: hd_p$,hd_tdate,hd_tcode,hd_tamount,mat hd_tg,hd_wr,hd_wu,hd_er,hd_eu,hd_gr,hd_gu,hd_tbal,hd_pcode eof HD_EOF
			if ~dupe(1) or p$=hd_p$ then
				if ~dupe(2) or tdate=hd_tdate then
					if ~dupe(3) or tamount=hd_tamount then
					  if ~dupe(4) or tcode=hd_tcode then
					    if trans1_rec<>rec(h_trans2) then
					      hd_return=1
					      goto HD_EOF
					    end if
					  end if
					end if
				end if
			end if
		loop
	end if  ! del_dupe   /   else
	HD_EOF: !
	fn_has_dupe=hd_return
fnend  ! fn_has_dupe
def library fntrans_delete(td_rec)
	autoLibrary
	fntrans_delete=fn_trans_delete(td_rec)
fnend
def fn_trans_delete(td_rec)
	if ~td_setup then
		td_setup=1
		open #h_td_trans1=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",i,outi,r
		open #h_td_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
		dim td_msg$(1)*90
		dim tdt_tg(11)
		dim tdc_tg(11)
	end if
	read #h_td_trans1,using Ftrans,rec=td_rec: td_customer_key$,td_tdate,td_trans_code,td_trans_amt,mat tdt_tg,td_wr,td_wu,td_er,td_eu,td_gr,td_gu,td_tbal,td_pcode noRec TD_XIT
	if td_pcode<>0 then
		mat td_msg$(0)
		fnAddOneC(mat td_msg$,'This transaction has already been posted (Posting Code='&str$(td_pcode)&') to General Ledger' 	)
		fnAddOneC(mat td_msg$,'The General Ledger must also be manually corrected.'                                             	)
		fnAddOneC(mat td_msg$,'You may instead consider a credit or debit memo.'                                                	)
		fnAddOneC(mat td_msg$,'Are you sure you want to delete it?'                                                              	)
		fnmsgbox(mat td_msg$,resp$,'',52)
		if resp$<>'Yes' then
			goto TD_XIT
		end if
	end if
	if td_trans_code=1 then ! Charge
		td_sign$='-' ! subtract it from balance
	else if td_trans_code=2 then ! Penalty
		td_sign$='-' ! subtract it from balance
	else if td_trans_code=3 then ! Collection
		td_sign$='+' ! add it to balance
	else if td_trans_code=4 then ! Credit Memo
		td_sign$='+' ! add it to balance
	else if td_trans_code=5 then ! Debit Memo
		td_sign$='-' ! subtract it from balance
	else
		mat td_msg$(2)
		td_msg$(1)='Unknown transaction type ('&str$(td_trans_code)&')'
		td_msg$(2)='Transaction may not be deleted'
		fnmsgbox(mat td_msg$,resp$,'',16)
		goto TD_XIT
	end if
	read #h_td_customer,using F_TB_CUSTOMER,key=td_customer_key$: tb_bal,mat tb_gb
	F_TB_CUSTOMER: form pos 292,pd 4.2,pos 388,10*pd 5.2
	if td_sign$='+' then
		tb_bal+=td_trans_amt
		for tb_item=1 to 10
			tb_gb(tb_item)=tb_gb(tb_item)+tdt_tg(tb_item)
			tdt_tg(tb_item)=0
		next tb_item
	else ! if td_sign$='-' then
		tb_bal-=td_trans_amt
		for tb_item=1 to 10
			tb_gb(tb_item)=tb_gb(tb_item)-tdt_tg(tb_item)
			tdt_tg(tb_item)=0
		next tb_item
	end if
	td_trans_amt=0
	! rewrite #h_td_customer,using F_TB_CUSTOMER,key=td_customer_key$: tb_bal,mat tb_gb
	pr #255: 'would delete rec '&str$(td_rec)
	! rewrite #h_td_trans1,using Ftrans,rec=td_rec: td_customer_key$,td_tdate,td_trans_code,td_trans_amt,mat tdt_tg,td_wr,td_wu,td_er,td_eu,td_gr,td_gu,td_tbal,td_pcode
	TD_XIT: !
fnend
def fn_header
	pr #255: "\qc {\b "&env$('cnam')&"}"
	pr #255: "\qc {\fs28 {\b "&env$('program_caption')&"}}"
	pr #255: "\qc {\b "&trim$(date$("MM/DD/CCYY"))&" }"
	pr #255: "\qr Page "&str$(p2+=1)
	pr #255: ""
	pr #255: "\ql {\ul   Record} {\ul Account   }  {\ul     Date} {\ul       Amount} {\ul}"
fnend
def fn_filter_add_chk(txt$*80,default_answer$; protected)
	fnChk(lc+=1,1,txt$)
	fnreg_read(sn$&'.'&txt$,resp$(respc+=1))
	if resp$(respc)='' then resp$(respc)=default_answer$
fnend
def fn_filter_get_chk(txt$,tf$)
	dgc_return=0
	if tf$='True' then dgc_return=1
	fnreg_write(sn$&'.'&txt$,tf$)
	fn_filter_get_chk=dgc_return
fnend
include: ertn
