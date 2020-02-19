00010 ! Replace S:\acsUB\Conversion\Bld_Trans
00020 ! Builds an ubTransVB from ubTrans.h, [Q]\UBmstr.h and ubAccTrn.h
00040 ! this program assumes the following:
00050 ! service 1 is Water
00060 ! service 2 is Sewer
00070 ! service 3 is Electric
00080 ! Service 4 is Gas
00090 ! Service 5 is Sanitation
00100 ! Service 6 is Fire Protection
00110 ! Service 7 is Merchandise
00120 ! Service 8 is Other
00130 !
00140   library 'S:\Core\Library': fnerror,fntop,fnTos,fnAcs,fnCmdSet,fnLbl,fndate_mmddyy_to_ccyymmdd,fnChk,fnxit,fnpause,fngethandle,fnub_index_customer,fnStatus
00150   on error goto Ertn
00160 !
00170   dim cap$*128,resp$(10)*80,g(11),acctrn_form$*80,rw4(22,13),key$*19,ru(6)
00180 !
00190   fntop("S:\acsUB\Conversion\Bld_Trans",cap$="Build Transactions")
00210 LOOP_STEP_1: ! 
00220   delubtransvb$="True"
00230 ! removebaddates$="True" ! Gosub MENU1
00240   fn_ub_build_transactions
00250   goto XIT
26000 MENU1: ! r:
26020   fnTos(sn$="bldtrans")
26040   fnLbl(1,1,"Convert Transactions")
26060   fnChk(4,1,"Delete existing transaction file before conversion") : resp$(1)="True"
26080   fnChk(5,1,"Remove Transactions with Bad Dates") : resp$(2)="False"
26100   fnCmdSet(2)
26120   fnAcs(sn$,0,mat resp$,ck)
26140   delubtransvb$=resp$(1) : removebaddates$=resp$(2)
26160   if ck=5 then pr 'cancel selected.  end reached - call support - conversion incomplete' : pause
26180 ! 
26200   return  ! /r
27000 XIT: chain "S:\acsUB\conversion\UBmstr-vb"
27010 IGNORE: continue 
28000   def library fnub_cnv_build_transactions
28020     library 'S:\Core\Library': fnerror,fntop,fnTos,fnAcs,fnCmdSet,fnLbl,fndate_mmddyy_to_ccyymmdd,fnChk,fnxit,fnpause,fngethandle,fnub_index_customer,fnStatus,fnindex_it
28060     delubtransvb$="True"
28080 ! removebaddates$="True" ! Gosub MENU1
28100     fnub_cnv_build_transactions=fn_ub_build_transactions
28120   fnend 
40000   def fn_ub_build_transactions
40020     fnStatus('Building Transactions...')
40040     if uprc$(delubtransvb$)=uprc$("True") and exists("[Q]\UBmstr\ubtransvb.h[cno]") then execute "Free [Q]\UBmstr\ubtransvb.h[cno] -n"
40060 ! 
40062     fnStatus('   * an error indexing ubindx5 on the next line is acceptable')
40080     fnub_index_customer
40100     open #master=3: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
40120 ! 
40140 ! open NEW files
40160     open #transvb=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,RecL=102,KPs=1,KLn=19,Use",internal,outIn,keyed 
40180 PHASE1: ! 
46000 ! r: convert transactions from ubAccTrn
46020     fnStatus('converting transactions from History Transactions (ubAccTrn.h[cno])')
46040     open #h_ubacctrn=1: "Name=[Q]\UBmstr\ubAccTrn.h[cno]",internal,outIn 
46060     if env$('client')='Franklinton' then 
46080       acctrn_form$='Form Pos 1,C 10,pd 4.2,x 2,n 6,n 1,n 1,10*pd 4.2' ! Franklinton only
46100     else if rln(h_ubacctrn)=64 or rln(h_ubacctrn)=72 then 
46120       acctrn_form$='Form Pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2'
46140     else if rln(h_ubacctrn)=62 or rln(h_ubacctrn)=70 then 
46160       acctrn_form$='Form Pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2'
46180     else if rln(h_ubacctrn)=68 then 
46200       acctrn_form$='Form Pos 1,C 10,pd 4.2,n 8,n 1,n 1,10*pd 4.2'
46220     else ! seems to be a problem
46240       pr 'unrecognised ubacctrn record length' : fnpause
46260     end if 
46280     fn_transaction_conv(h_ubacctrn)
46300 ! /r
48000 ! r: convert transactions from ubTrans
48020     fnStatus('converting transactions from Current Transactions (ubTrans.h[cno])')
48040     open #h_ubtrans=fngethandle: "Name=[Q]\UBmstr\ubTrans.h[cno]",internal,input 
48060     fn_transaction_conv(h_ubtrans)
48080 ! /r
50000 ! r: convert transactions 13 month history
50020     fnStatus('converting charge (only) transactions from 13 month history (deletes previously made matching entries)')
50040     restore #master: 
50060     do 
50080       read #master,using 'form pos 1,c 10,pos 438,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2': p$,mat rw4 eof PHASE4
50100       for month=1 to 13
50120         tdate=fndate_mmddyy_to_ccyymmdd(rw4(8,month))
50140         if tdate<>0 and tdate<>20000000 then 
50160           g(01)=rw4(09,month)
50180           g(02)=rw4(10,month)
50200           g(03)=rw4(11,month)
50220           g(04)=rw4(12,month)
50240           g(05)=rw4(13,month)
50260           g(06)=rw4(14,month)
50280           g(07)=rw4(15,month)
50300           g(08)=rw4(16,month)
50320           g(09)=rw4(17,month)
50340           g(10)=rw4(18,month)
50360           g(11)=rw4(19,month)
50380           if transcode=1 then g(10)=0 ! don't include penalties unless they are needed to total to the transaction amount
50400           ru(1)=rw4(1,month)
50420           ru(2)=rw4(2,month)
50440           ru(3)=rw4(3,month)
50460           ru(4)=rw4(4,month)
50480           ru(5)=rw4(5,month)
50500           ru(6)=rw4(6,month)
50520           bal=rw4(7,month)
50540           postcode=9
50560           transcode=1
50580           tamt=rw4(19,month) ! <--rw4(19,month) is amount billed that month, rw4(22,month) is the total amount collected that month
50600           ru(3)=ru(4)=0 !  (no electric)
50620           key$=p$&lpad$(str$(tdate),8)&str$(transcode)
51000 ! r:         delete previously converted matching transaction(s)
51020 !         delete the first matching (charge) transaction
51040 !         do
51060           read #transvb,using 'form pos 1,C 10',key=key$: p$ nokey EO_DELETE_MATCHING_TRANS
51080 !           read #transvb,using 'form pos 1,C 10,pos 19,2*C 1',key=key$: p$,trans_code$,posting_code$ nokey EO_DELETE_MATCHING_TRANS
51100 !           trans_code=0 : trans_code=val(trans_code$) : conv ignore
51120 !           posting_code=0 : posting_code=val(posting_code$) : conv ignore
51140 !           if trans_code=1 then
51160           delete #transvb: 
51180 !           end if
51200 !         loop until trans_code=1
51220 !         do ! delete all matching transactions
51240 !           read #transvb,using 'form pos 1,C 10,pos 20,2*n 1',key=key$: p$,posting_code nokey EO_DELETE_MATCHING_TRANS
51260 !           if posting_code=5 then
51280 !             delete #transvb:
51300 !           end if
51320 !           rewrite #transvb,using 'form pos 24,11*pd 4.2,6*pd 5,pd 4.2,n 1',key=key$: mat g,mat ru,bal,postcode
51340 !         loop
51360 !        goto NEXT_MONTH
51380 EO_DELETE_MATCHING_TRANS: !  /r
52000           fn_fix_trans_breakdown(mat g,tamt)
52020 !         if sum(g)=tamt then ! SKIP ANY TRANSACTIONS THAT DON'T ADD UP
52040           write #transvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,mat ru,bal,postcode
52060 !         if trim$(p$)='209740.00' and month=1 then pr 'tdate=';tdate; 'tamt=';tamt : for x=1 to udim(mat rw4,1) : pr rw4(x,1) : next x :  pause
52080           write_count+=1
52100 !         end if
52120         end if  ! tdate<>0 and tdate<>20000000
52140 ! NEXT_MONTH: !
52160       next month
52180     loop 
52200 PHASE4: ! 
52220 ! /r
54000     close #master: 
54040     close #transvb: 
54060     fnindex_it("[Q]\UBmstr\UBTransvb.h[cno]", "[Q]\UBmstr\UBTrindx.h[cno]","1 19")
54080     if removebaddates$="True" then let fn_removebaddates
54100     fnStatus('    Build Transaction - write_count='&str$(write_count))
54120     fnStatus('Building Transactions complete.')
54140   fnend  ! fn_ub_build_transactions
56000   def fn_translate_transcode
56020     if transcode=1 and postcode=4 then 
56040       transcode=5 ! Debit Memo
56060     else if transcode=1 and postcode<>4 then 
56080       transcode=1 ! charge
56100     else if transcode=2 then 
56120       transcode=2 ! penalty
56140     else if transcode=3 then 
56160       transcode=3 ! collection
56180     else if transcode=4 then 
56200       transcode=4 ! Credit Memo
56220     end if  ! end of translate transcode
56240   fnend  ! fn_translate_transcode
58000   def fn_removebaddates
58020     open #transvb=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,RecL=102,KPs=1,KLn=19,Use",internal,outIn,keyed 
58040     do 
58060       read #transvb,using "Form Pos 11,N 8": tdate eof TRANSVB_EOF
58080       tdate$=str$(tdate)
58100       if val(tdate$(1:4))<1950 or val(tdate$(1:4))>2049 or val(tdate$(5:6))<1 or val(tdate$(5:6))>12 or val(tdate$(7:8))<1 or val(tdate$(7:8))>31 then 
58120         delete #transvb: 
58140       end if 
58160     loop 
58180 TRANSVB_EOF: ! 
58200     close #transvb: 
58220   fnend  ! fn_removebaddates
60000   def fn_transaction_conv(h_trans)
60020     do 
60040 READ_TRANS: ! 
60060       mat g=(0)
60080       tamt=tdate=transcode=postcode=0
60100       p$=''
60120       if rln(h_trans)=23 then 
60140         read #h_trans,using 'form pos 1,c 10,pd 4.2,pd 4,2*n 1': p$,tamt,tdate,transcode,postcode eof TC_FINIS
60160       else 
60180         read #h_trans,using acctrn_form$: p$,tamt,tdate,transcode,postcode,g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10) eof TC_FINIS ioerr L90000 ! ,ioerr L90000,conv L90000
60200       end if 
60210 !     if trim$(p$)='209740.00' and tamt=54.18 then pause
60220       read #master,using 'form pos 1,c 10,pos 296,pd 4,11*pd 4.2',key=p$: z$,fdate,mat g nokey READ_TRANS
60240       if sum(g)-g(10)=tamt or transcode=1 then g(10)=0 ! don't include penaltiies unless they are needed to total to the transaction amount
60260       if transcode=2 and g(10)=tamt then g(1)=g(2)=g(3)=g(4)=g(5)=g(6)=g(7)=g(8)=g(9)=0 ! make sure all other charges are zero on penalty records where g(10) is the penalty field
60280       fn_translate_transcode
60282       if postcode=5 then goto READ_TRANS ! Skip all transaction code 5s.
60300       if len(str$(tdate))<=6 then tdate=fndate_mmddyy_to_ccyymmdd(tdate)
60320       if len(str$(fdate))<=6 then fdate=fndate_mmddyy_to_ccyymmdd(fdate)
60340 ! 
60360 ! if tdate=20415 then pause
60380 ! 
60400       postcode=9
60420       if tdate=fdate and transcode=1 then mat tg=g(1:10) else mat tg=(0)
60440 !     read #transvb,using 'form pos 1,C 10,pos 20,pd 4.2',key=rpad$(p$&str$(tdate)&str$(transcode),kln(transvb)): p$,tamt_compare nokey TC_MATCHING_TRAN_NOKEY
60460 !     if tamt_compare=tamt  then goto READ_TRANS  ! skip if trans already exists (matches on transcode, account, date and amount)
60480 TC_MATCHING_TRAN_NOKEY: ! 
60490       fn_fix_trans_breakdown(mat tg,tamt)
60500       write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat tg,d1,d3,empty,empty,d9,d10,bal,postcode
60520       write_count+=1
60560 !    goto READ_TRANS
60580 !     fn_fix_trans_breakdown(mat g,tamt)
60600 !     write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,d1,d3,d,d,d9,d10,bal,postcode
60620 !     write_count+=1
60640     loop 
60660 L90000: ! 
60680     reread #h_ubacctrn,using acctrn_form$: p$
60700     continue 
60720 TC_FINIS: ! 
60740 ! fnpause
60760     close #h_trans,free: 
60780   fnend 
62000   def fn_fix_trans_breakdown(mat g,tamt)
62020     if sum(g)><tamt then !  transactions don't add up
62040       if env$('client')='French Settlement' then 
62060         g(8)=g(8)+tamt-sum(g) ! put the difference into g(8) - OTHER
62080       else 
62100         g(10)=g(10)+tamt-sum(g) ! put the difference into g(10)
62120       end if 
62140     end if 
62160   fnend 
76220 ! <updateable region: ertn>
76240 ERTN: fnerror(program$,err,line,act$,"xit")
76260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76280   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76320 ERTN_EXEC_ACT: execute act$ : goto ERTN
76340 ! </updateable region: ertn>
