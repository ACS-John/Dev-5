! Replace S:\acsUB\Conversion\Bld_Trans
! Builds an ubTransVB from ubTrans.h, [Q]\UBmstr.h and ubAccTrn.h
! this program assumes the following:
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
 
	dim cap$*128,resp$(10)*80,g(11),acctrn_form$*80,rw4(22,13),key$*19,xru(6)
 
	fnTop(program$,cap$="Build Transactions")
LOOP_STEP_1: !
	delubtransvb$='True'
! removebaddates$='True' ! Gosub MENU1
	fn_ub_build_transactions
	goto Xit
MENU1: ! r:
	fnTos
	fnLbl(1,1,"Convert Transactions")
	fnChk(4,1,"Delete existing transaction file before conversion") : resp$(1)='True'
	fnChk(5,1,"Remove Transactions with Bad Dates") : resp$(2)='False'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	delubtransvb$=resp$(1) : removebaddates$=resp$(2)
	if ckey=5 then pr 'cancel selected.  end reached - call support - conversion incomplete' : pause
 
return  ! /r
Xit: chain "S:\acsUB\conversion\UBmstr-vb"
def library fnub_cnv_build_transactions
	autoLibrary
	delubtransvb$='True'
	! removebaddates$='True' ! Gosub MENU1
	fnub_cnv_build_transactions=fn_ub_build_transactions
fnend
def fn_ub_build_transactions
		fnStatus('Building Transactions...')
		if uprc$(delubtransvb$)=uprc$('True') and exists("[Q]\UBmstr\ubtransvb.h[cno]") then execute "Free [Q]\UBmstr\ubtransvb.h[cno] -n"
 
		fnStatus('   * an error indexing ubindx5 on the next line is acceptable')
		fnub_index_customer
		open #master=3: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
 
		! open NEW files
		open #transvb=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,RecL=102,KPs=1,KLn=19,Use",internal,outIn,keyed
    ! r: convert transactions from ubAccTrn
		fnStatus('converting transactions from History Transactions (ubAccTrn.h[cno])')
		open #h_ubacctrn=1: "Name=[Q]\UBmstr\ubAccTrn.h[cno]",internal,outIn
		if env$('client')='Franklinton' then
			acctrn_form$='form pos 1,C 10,pd 4.2,x 2,n 6,n 1,n 1,10*pd 4.2' ! Franklinton only
		else if rln(h_ubacctrn)=64 or rln(h_ubacctrn)=72 then
			acctrn_form$='form pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2'
		else if rln(h_ubacctrn)=62 or rln(h_ubacctrn)=70 then
			acctrn_form$='form pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2'
		else if rln(h_ubacctrn)=68 then
			acctrn_form$='form pos 1,C 10,pd 4.2,n 8,n 1,n 1,10*pd 4.2'
		else ! seems to be a problem
			pr 'unrecognised ubacctrn record length' : fnpause
		end if
		fn_transaction_conv(h_ubacctrn)
! /r
! r: convert transactions from ubTrans
		fnStatus('converting transactions from Current Transactions (ubTrans.h[cno])')
		open #h_ubtrans=fnH: "Name=[Q]\UBmstr\ubTrans.h[cno]",internal,input
		fn_transaction_conv(h_ubtrans)
! /r
! r: convert transactions 13 month history
		fnStatus('converting charge (only) transactions from 13 month history (deletes previously made matching entries)')
		restore #master:
		do
			read #master,using 'form pos 1,c 10,pos 438,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2': p$,mat rw4 eof PHASE4
			for month=1 to 13
				tdate=fndate_mmddyy_to_ccyymmdd(rw4(8,month))
				if tdate<>0 and tdate<>20000000 then
					g(01)=rw4(09,month)
					g(02)=rw4(10,month)
					g(03)=rw4(11,month)
					g(04)=rw4(12,month)
					g(05)=rw4(13,month)
					g(06)=rw4(14,month)
					g(07)=rw4(15,month)
					g(08)=rw4(16,month)
					g(09)=rw4(17,month)
					g(10)=rw4(18,month)
					g(11)=rw4(19,month)
					if transcode=1 then g(10)=0 ! don't include penalties unless they are needed to total to the transaction amount
					xru(1)=rw4(1,month)
					xru(2)=rw4(2,month)
					xru(3)=rw4(3,month)
					xru(4)=rw4(4,month)
					xru(5)=rw4(5,month)
					xru(6)=rw4(6,month)
					bal=rw4(7,month)
					postcode=9
					transcode=1
					tamt=rw4(19,month) ! <--rw4(19,month) is amount billed that month, rw4(22,month) is the total amount collected that month
					xru(3)=xru(4)=0 !  (no electric)
					key$=p$&lpad$(str$(tdate),8)&str$(transcode)
! r:         delete previously converted matching transaction(s)
!         delete the first matching (charge) transaction
!         do
					read #transvb,using 'form pos 1,C 10',key=key$: p$ nokey EO_DELETE_MATCHING_TRANS
!           read #transvb,using 'form pos 1,C 10,pos 19,2*C 1',key=key$: p$,trans_code$,posting_code$ nokey EO_DELETE_MATCHING_TRANS
!           trans_code=0 : trans_code=val(trans_code$) : conv ignore
!           posting_code=0 : posting_code=val(posting_code$) : conv ignore
!           if trans_code=1 then
					delete #transvb:
!           end if
!         loop until trans_code=1
!         do ! delete all matching transactions
!           read #transvb,using 'form pos 1,C 10,pos 20,2*n 1',key=key$: p$,posting_code nokey EO_DELETE_MATCHING_TRANS
!           if posting_code=5 then
!             delete #transvb:
!           end if
!           rewrite #transvb,using 'form pos 24,11*pd 4.2,6*pd 5,pd 4.2,n 1',key=key$: mat g,mat xru,bal,postcode
!         loop
!        goto NEXT_MONTH
EO_DELETE_MATCHING_TRANS: !  /r
					fn_fix_trans_breakdown(mat g,tamt)
!         if sum(g)=tamt then ! skip ANY TRANSACTIONS THAT DON'T ADD UP
					write #transvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,mat xru,bal,postcode
!         if trim$(p$)='209740.00' and month=1 then pr 'tdate=';tdate; 'tamt=';tamt : for x=1 to udim(mat rw4,1) : pr rw4(x,1) : next x :  pause
					write_count+=1
!         end if
				end if  ! tdate<>0 and tdate<>20000000
! NEXT_MONTH: !
			next month
		loop
PHASE4: !
! /r
		close #master:
		close #transvb:
		fnIndex("[Q]\UBmstr\UBTransvb.h[cno]", "[Q]\UBmstr\UBTrindx.h[cno]","1 19")
		if removebaddates$='True' then let fn_removebaddates
		fnStatus('    Build Transaction - write_count='&str$(write_count))
		fnStatus('Building Transactions complete.')
fnend  ! fn_ub_build_transactions
def fn_translate_transcode
	if transcode=1 and postcode=4 then
		transcode=5 ! Debit Memo
	else if transcode=1 and postcode<>4 then
		transcode=1 ! charge
	else if transcode=2 then
		transcode=2 ! penalty
	else if transcode=3 then
		transcode=3 ! collection
	else if transcode=4 then
		transcode=4 ! Credit Memo
	end if  ! end of translate transcode
fnend  ! fn_translate_transcode
def fn_removebaddates
	open #transvb=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,RecL=102,KPs=1,KLn=19,Use",internal,outIn,keyed
	do
		read #transvb,using "form pos 11,N 8": tdate eof TRANSVB_EOF
		tdate$=str$(tdate)
		if val(tdate$(1:4))<1950 or val(tdate$(1:4))>2049 or val(tdate$(5:6))<1 or val(tdate$(5:6))>12 or val(tdate$(7:8))<1 or val(tdate$(7:8))>31 then
			delete #transvb:
		end if
	loop
	TRANSVB_EOF: !
	close #transvb:
fnend  ! fn_removebaddates
def fn_transaction_conv(h_trans)
	do
		READ_TRANS: !
		mat g=(0)
		tamt=tdate=transcode=postcode=0
		p$=''
		if rln(h_trans)=23 then
			read #h_trans,using 'form pos 1,c 10,pd 4.2,pd 4,2*n 1': p$,tamt,tdate,transcode,postcode eof TC_FINIS
		else
			read #h_trans,using acctrn_form$: p$,tamt,tdate,transcode,postcode,g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10) eof TC_FINIS ioerr L90000 ! ,ioerr L90000,conv L90000
		end if
		!   if trim$(p$)='209740.00' and tamt=54.18 then pause
		read #master,using 'form pos 1,c 10,pos 296,pd 4,11*pd 4.2',key=p$: z$,fdate,mat g nokey READ_TRANS
		if sum(g)-g(10)=tamt or transcode=1 then g(10)=0 ! don't include penaltiies unless they are needed to total to the transaction amount
		if transcode=2 and g(10)=tamt then g(1)=g(2)=g(3)=g(4)=g(5)=g(6)=g(7)=g(8)=g(9)=0 ! make sure all other charges are zero on penalty records where g(10) is the penalty field
		fn_translate_transcode
		if postcode=5 then goto READ_TRANS ! skip all transaction type 5s.
		if len(str$(tdate))<=6 then tdate=fndate_mmddyy_to_ccyymmdd(tdate)
		if len(str$(fdate))<=6 then fdate=fndate_mmddyy_to_ccyymmdd(fdate)
 
		! if tdate=20415 then pause
 
		postcode=9
		if tdate=fdate and transcode=1 then mat tg=g(1:10) else mat tg=(0)
		!     read #transvb,using 'form pos 1,C 10,pos 20,pd 4.2',key=rpad$(p$&str$(tdate)&str$(transcode),kln(transvb)): p$,tamt_compare nokey TC_MATCHING_TRAN_NOKEY
		!     if tamt_compare=tamt  then goto READ_TRANS  ! skip if trans already exists (matches on transcode, account, date and amount)
		TC_MATCHING_TRAN_NOKEY: !
		fn_fix_trans_breakdown(mat tg,tamt)
		write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat tg,d1,d3,empty,empty,d9,d10,bal,postcode
		write_count+=1
		!    goto READ_TRANS
		!     fn_fix_trans_breakdown(mat g,tamt)
		!     write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,d1,d3,d,d,d9,d10,bal,postcode
		!     write_count+=1
	loop
	L90000: !
	reread #h_ubacctrn,using acctrn_form$: p$
	continue
	TC_FINIS: !
	! fnpause
		close #h_trans,free:
fnend
def fn_fix_trans_breakdown(mat g,tamt)
	if sum(g)><tamt then !  transactions don't add up
		if env$('client')='French Settlement' then
			g(8)=g(8)+tamt-sum(g) ! put the difference into g(8) - OTHER
		else
			g(10)=g(10)+tamt-sum(g) ! put the difference into g(10)
		end if
	end if
fnend
include: ertn
