00030   ! ______________________________________________________________________
00040   if ~setup_library then let fn_setup_library
00050   on error goto Ertn
00060   ! ______________________________________________________________________
00070   dim z$*10,cap$*128,txt$*40,tg(11),resp$(10)*80
00090   fntop(program$,cap$="Duplicate Transaction Report")
00100   ! ______________________________________________________________________
00110   open #fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed 
00120   open #h_trans1:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",internal,input,relative 
00130   open #h_trans2:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,input,keyed 
00140   trans1_lrec=lrec(h_trans1)
00150   F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00160   ! 
00170   del_dupe=1
00180   ! 
00190   fnTos(sn$='DupTr3') : respc=lc=0
00200   fn_filter_add_chk('Account','True')
00210   fn_filter_add_chk('Transaction Date','False')
00220   fn_filter_add_chk('Amount','True')
00230   fn_filter_add_chk('Transaction Code','True')
00240   lc+=1
00250   fnLbl(lc+=1,1,"Starting Record:",16,1)
00260   fnTxt(lc,18,10,0,0,'30')
00270   resp$(respc+=1)=str$(max(1,trans1_lrec-1000))
00280   fnLbl(lc+=1,1,"Ending Record:",16,1)
00290   fnTxt(lc,18,10,0,0,'30')
00300   resp$(respc+=1)=str$(trans1_lrec)
00310   fnCmdSet(2)
00320   fnAcs(sn$,0,mat resp$,ckey)
00330   if ckey=5 then goto XIT
00340   respc=0
00350   dupe(1)=fn_filter_get_chk('Account',resp$(respc+=1))
00360   dupe(2)=fn_filter_get_chk('Transaction Date',resp$(respc+=1))
00370   dupe(3)=fn_filter_get_chk('Amount',resp$(respc+=1))
00380   dupe(4)=fn_filter_get_chk('Transaction Code',resp$(respc+=1))
00390   rec_start=val(resp$(respc+=1))
00400   rec_end=val(resp$(respc+=1))
00410   fnopenprn
00420   fn_header
00430   ! restore #h_trans1,rec=rec_start: noRec NEXT_REC
00440   trans1_rec=rec_start-1
00450   do 
00460     NEXT_REC: ! 
00470     trans1_rec+=1
00480     if trans1_rec>trans1_lrec or (rec_end>0 and rec_end<trans1_rec) then goto FINIS
00490     read #h_trans1,using F_TRANS,rec=trans1_rec: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode noRec NEXT_REC
00500     pr trans1_rec
00520     if fn_has_dupe then 
00530       pr #255,using FORM_OUT: trans1_rec,p$,tdate,tamount pageoflow PGOF
00540       FORM_OUT: form n 8,x 1,c 10,x 1,x 1,pic(zzzz/zz/zz),n 11.2
00550     end if  ! fn_has_dupe(z$)
00560   loop 
00570 ! ______________________________________________________________________
00580 FINIS: ! 
00590   fncloseprn
00600   close #h_trans1: 
00610 XIT: ! 
00620   fnxit
00630 ! ______________________________________________________________________
00640 PGOF: ! r:
00650   pr #255: newpage
00660   fn_header
00670 continue ! /r
00680 def fn_setup_library
00690   library 'S:\Core\Library': fnxit,fnAcs,fnLbl,fnwait,fnTos,fnTxt,fnerror,fndate_mmddyy_to_ccyymmdd,fnCmdSet,fntop,fncmbact,fngethandle,fncloseprn,fnopenprn,fnChk,fnreg_read,fnreg_write,fnmsgbox
00700 fnend  ! fn_setup_library
00710 def fn_has_dupe
00720   hd_return=0
00730   dim hd_tg(11)
00740   if del_dupe then 
00750     restore #h_trans2: 
00760     do 
00770       read #h_trans2,using F_TRANS: hd_p$,hd_tdate,hd_tcode,hd_tamount,mat hd_tg,hd_wr,hd_wu,hd_er,hd_eu,hd_gr,hd_gu,hd_tbal,hd_pcode eof HD_EOF
00780       if ~dupe(1) or p$=hd_p$ then 
00790         if ~dupe(2) or tdate=hd_tdate then 
00800           if ~dupe(3) or tamount=hd_tamount then 
00810             if ~dupe(4) or tcode=hd_tcode then 
00820               if trans1_rec<>rec(h_trans2) then 
00830                 hd_return=1
00840                 fn_trans_delete(rec(h_trans2))
00850               end if 
00860             end if 
00870           end if 
00880         end if 
00890       end if 
00900     loop 
00910   else 
00920     restore #h_trans2: 
00930     do 
00940       read #h_trans2,using F_TRANS: hd_p$,hd_tdate,hd_tcode,hd_tamount,mat hd_tg,hd_wr,hd_wu,hd_er,hd_eu,hd_gr,hd_gu,hd_tbal,hd_pcode eof HD_EOF
00950       if ~dupe(1) or p$=hd_p$ then 
00960         if ~dupe(2) or tdate=hd_tdate then 
00970           if ~dupe(3) or tamount=hd_tamount then 
00980             if ~dupe(4) or tcode=hd_tcode then 
00990               if trans1_rec<>rec(h_trans2) then 
01000                 hd_return=1
01010                 goto HD_EOF
01020               end if 
01030             end if 
01040           end if 
01050         end if 
01060       end if 
01070     loop 
01080   end if  ! del_dupe   /   else 
01090   HD_EOF: ! 
01100   fn_has_dupe=hd_return
01110 fnend  ! fn_has_dupe
01120 def library fntrans_delete(td_rec)
01130   if ~setup_library then let fn_setup_library
01140   fntrans_delete=fn_trans_delete(td_rec)
01150 fnend 
01160 def fn_trans_delete(td_rec)
01170   if ~td_setup then 
01180     td_setup=1
01190     open #h_td_trans1:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",internal,outIn,relative 
01200     open #h_td_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
01210     dim td_msg$(1)*90
01220     dim tdt_tg(11)
01230     dim tdc_tg(11)
01240   end if 
01250   read #h_td_trans1,using F_TRANS,rec=td_rec: td_customer_key$,td_tdate,td_trans_code,td_trans_amt,mat tdt_tg,td_wr,td_wu,td_er,td_eu,td_gr,td_gu,td_tbal,td_pcode noRec TD_XIT
01260   if td_pcode<>0 then 
01270     mat td_msg$(4)
01280     td_msg$(1)='This transaction has already been posted (Posting Code='&str$(td_pcode)&') to General Ledger'
01290     td_msg$(2)='The General Ledger must also be manually corrected.'
01300     td_msg$(3)='You may instead consider a credit or debit memo.'
01310     td_msg$(4)='Are you sure you want to delete it?'
01320     fnmsgbox(mat txt$,resp$,cap$,52)
01330     if resp$<>'Yes' then 
01340       goto TD_XIT
01350     end if 
01360   end if 
01370   if td_trans_code=1 then ! Charge
01380     td_sign$='-' ! subtract it from balance
01390   else if td_trans_code=2 then ! Penalty
01400     td_sign$='-' ! subtract it from balance
01410   else if td_trans_code=3 then ! Collection
01420     td_sign$='+' ! add it to balance
01430   else if td_trans_code=4 then ! Credit Memo
01440     td_sign$='+' ! add it to balance
01450   else if td_trans_code=5 then ! Debit Memo
01460     td_sign$='-' ! subtract it from balance
01470   else 
01480     mat td_msg$(2)
01490     td_msg$(1)='Unknown transaction code ('&str$(td_trans_code)&')'
01500     td_msg$(2)='Transaction may not be deleted'
01510     fnmsgbox(mat txt$,resp$,cap$,16)
01520     goto TD_XIT
01530   end if 
01540   read #h_td_customer,using F_TB_CUSTOMER,key=td_customer_key$: tb_bal,mat tb_gb
01550   F_TB_CUSTOMER: form pos 292,pd 4.2,pos 388,10*pd 5.2
01560   if td_sign$='+' then 
01570     tb_bal+=td_trans_amt
01580     for tb_item=1 to 10
01590       tb_gb(tb_item)=tb_gb(tb_item)+tdt_tg(tb_item)
01600       tdt_tg(tb_item)=0
01610     next tb_item
01620   else ! if td_sign$='-' then
01630     tb_bal-=td_trans_amt
01640     for tb_item=1 to 10
01650       tb_gb(tb_item)=tb_gb(tb_item)-tdt_tg(tb_item)
01660       tdt_tg(tb_item)=0
01670     next tb_item
01680   end if 
01690   td_trans_amt=0
01700   ! .! rewrite #h_td_customer,using F_TB_CUSTOMER,key=td_customer_key$: tb_bal,mat tb_gb
01710   pr #255: 'would delete rec '&str$(td_rec)
01720   ! .! rewrite #h_td_trans1,using F_TRANS,rec=td_rec: td_customer_key$,td_tdate,td_trans_code,td_trans_amt,mat tdt_tg,td_wr,td_wu,td_er,td_eu,td_gr,td_gu,td_tbal,td_pcode
01730   TD_XIT: ! 
01740 fnend 
01750 def fn_header
01760   pr #255: "\qc {\b "&env$('cnam')&"}"
01770   pr #255: "\qc {\fs28 {\b "&cap$&"}}"
01780   pr #255: "\qc {\b "&trim$(date$("MM/DD/CCYY"))&" }"
01790   pr #255: "\qr Page "&str$(p2+=1)
01800   pr #255: ""
01810   pr #255: "\ql {\ul   Record} {\ul Account   }  {\ul     Date} {\ul       Amount} {\ul}"
01820 fnend 
01830 def fn_filter_add_chk(txt$*80,default_answer$; protected)
01840   fnChk(lc+=1,1,txt$)
01850   fnreg_read(sn$&'.'&txt$,resp$(respc+=1))
01860   if resp$(respc)='' then resp$(respc)=default_answer$
01870 fnend 
01880 def fn_filter_get_chk(txt$,tf$)
01890   dgc_return=0
01900   if tf$='True' then dgc_return=1
01910   fnreg_write(sn$&'.'&txt$,tf$)
01920   fn_filter_get_chk=dgc_return
01930 fnend  ! fn_filter_get_chk
01950 ! <Updateable Region: ERTN>
01960 ERTN: fnerror(program$,err,line,act$,"xit")
01970   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02000 ERTN_EXEC_ACT: execute act$ : goto ERTN
02010 ! /region
