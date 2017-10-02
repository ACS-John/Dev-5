10010 ! pr checks
10020 ! ______________________________________________________________________
10030   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fntos,fnlbl,fntxt,fncomboa,fnchk,fncmdset,fnacs,fncombof,fnfra,fnmsgbox,fnbutton,fnopt,fncmdkey,fnaddpayee,fnqgl,fnagl$,fnrgl$,fncreg_read,fncreg_write
10040   on error goto ERTN
10050 ! ______________________________________________________________________
10060   dim vn$*8,holdvn$*8,up$(4),amt(15,3),iv$(15,3),de$(15,3)*13,ivdate(15,3)
10070   dim cnam$*40,tr(2),misc$(10)*20,miscgl$(10)*12
10080   dim de$*50,lcn$*8,whgl$(5)*12,gl$*12,allocde$*30
10090   dim whgl(5,3),in3$(150)*30,bn$*30,b$(4)*30
10100   dim ade$*30,dedcode(10),agl(3),de$*50
10110   dim t1(5),arec(100),d(2)
10120   dim pr$(4)*30,myact$*20,disamt(15,3)
10130   dim resp$(50)*50,item4$(2)*35,item5$(2)*35
10140   dim holdpayee$*50,holdresp$(94)*50,contact$*30,email$*50,fax$*12
10150   dim inl$(4)*50,ml$(5)*90
10160   dim allockey$*20
10170   dim eng$*128,wording$(27)*9,amount(11)
10180   dim tr$(5)*35,sn$*30,dtr$(5)*35,payeegl$*12,gldesc$*30
10190 ! ______________________________________________________________________
10200   let fntop(program$)
10230   let prd=val(date$(4:5)&date$(7:8)&date$(1:2))
10240   open #bankmstr:=12: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
10250   open #h_paymstr1:=13: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
10260   open #paymstr2:=14: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
10270   open #trmstr1:=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
10280   open #trmstr2:=2: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
10290   open #tralloc:=3: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\tralloc-idx.h"&env$('cno')&",Shr",internal,outin,keyed 
10300   open #h_paytrans:=4: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
10310   open #h_unpdaloc:=7: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\uaidx2.h"&env$('cno')&",Shr",internal,outin,keyed 
10320   open #glmstr18:=18: "Name="&env$('Q')&"\CLmstr\GLmstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
10330   open #glcontrol:=19: "Name="&env$('Q')&"\CLmstr\Fundmstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Fundidx1.h"&env$('cno')&",Shr",internal,outin,keyed 
10340   open #ivpaid:=6: "Name="&env$('Q')&"\CLmstr\IvPaid.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\IVIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
10350   open #payeegl:=17: "Name="&env$('Q')&"\CLmstr\payeeGLBreakdown.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Payeeglbkdidx.h"&env$('cno')&",Shr",internal,outin,keyed 
10360   let fn_get_coinfo
10370 MENU1: ! 
10380   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
10390   let t1(1)=bal
10400   let upi=t1(5)
10410   let t1(3)=t1(1)-t1(2)
10420   ckn=val(lcn$)+1 conv ignore
10430   bn$=rtrm$(bn$)
10440 MAIN_QUESTIONS: ! 
10450   if fn_scr_main_questions=5 then goto XIT
10460 ! 
10470   let tac=0
10480   if ti1=3 and pri=1 then let h_vf1=23 else let h_vf1=h_paymstr1
10490   if ti1=1 then let h_vf1=13
10500   allign=0
10510   if ti1=3 then goto REPRINT_CHECKS
10520   open #company=15: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
10530   rewrite #company,using 'Form POS 152,N 2',rec=1: bankcode
10540   close #company: 
10550   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
10560   mat in3$=("")
10570   bn$=rtrm$(bn$)
10580   if ti1=1 or ti1=3 then goto CKOPTION1_CHECK_ENTRY
10590   restore #h_paytrans,key>=lpad$(rtrm$(begvn$),8)&"            ": nokey MENU1
10600   amt=arec=x=y=0
10610   mat amt=(0) : mat de$=("")
10620 READ_4: ! 
10630   read #h_paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,n 1,n 6,n 10.2,n 8',release: vn$,iv$,mat up$,upa,pcde,bc,ck$,dp,gde,pdate,disamt,ddate eof EOF_ROUTINE
10640   let vn$=lpad$(rtrm$(vn$),8)
10650   if rtrm$(vn$)="" then goto READ_4
10660   if pcde=0 then goto READ_4
10670   if bc=0 then bc=bankcode ! if coded for payment with no bank code, pay from the  current bank account
10680   if bc<>bankcode then goto READ_4
10690   let lr4=rec(h_paytrans) ! what to do
10700   if holdvn$=vn$ or rtrm$(holdvn$)="" then goto L1130 else gosub SUB_PRINT_CHECK
10710 L1130: if arec>30 then gosub SUB_PRINT_CHECK
10720   let fn_checkdiscount
10730   if iv$<>hiv$ then 
10740     let y=y+1
10750     if y>2 then let y=1
10760     if y=1 then let x+=1
10770     if x>15 then gosub SUB_PRINT_CHECK
10780     let iv$(x,y)=iv$(1:12)
10790     let de$(x,y)=up$(4)(1:13) ! this one for printing from unpaid file
10800   end if 
10810   amt(x,y)=amt(x,y)+upa
10820   let ivdate(x,y)=dp
10830   let disamt(x,y)=disamt
10840   arec+=1
10850   arec(arec)=lr4
10860   let holdvn$=vn$
10870   let hiv$=iv$
10880   amt=sum(amt)
10890   let st1=1
10900   goto READ_4
10910 ! ______________________________________________________________________
10920 SUB_PRINT_CHECK: ! r:
10930   let fn_cknum
10940 ! if env$('client')="Washington Parrish" then let fnprocess(1) ! skip Atlantis screen
10950   let fnopenprn(cp,42,220,process)
10970   ckn1=ckn
10980 !   on ckoption goto L1360,L1360 none L1360 ! L1390
10990 ! L1360: ! 
11000   if amt<=0 then goto L2420
11020   if scc$="CSS" then let fn_portion_check : let fn_portion_stub(1) : let fn_portion_stub(2)
11030 ! L1390: ! 
11040   if scc$="SCS" then let fn_portion_stub(1) : let fn_portion_check : let fn_portion_stub(2)
11060   if scc$="SSC" then let fn_portion_stub(1) : let fn_portion_stub(2) : let fn_portion_check
11070   if scc$="SCC" then let fn_portion_stub(1) : let fn_portion_check : let fn_portion_check
11090   gosub UPDATEINVOICE
11100 return ! /r
11110 ! ______________________________________________________________________
11120 UPDATEINVOICE: ! r:
11130   for j=1 to arec
11140     rewrite #h_paytrans,using 'Form POS 76,N 8,N 6',rec=arec(j): ckn,prdmmddyy
11150   next j
11160   let idx=1
11170   ! allign=3  !  remove allign routine alltogether   <-- that did not work.
11180   if allign=3 then pr #255: newpage : goto ALIGN_COMPLETED
11190   pr #255: newpage
11200   let fncloseprn
11210 ! if env$('client')="Washington Parrish" then let fnprocess(0)
11220   let holdpayee$=""
11230   if ti1=1 then ckoption=1 : allign=2 : goto L2300 ! skip the continue routine when entering and printing checks
11240   if ~allign then 
11250     if ckoption=1 or ckoption=3 then 
11260       mat inl$(4)
11270       let inl$(4)="4. Void previous check    "
11280     else if ckoption=2 then 
11290       mat inl$(3)
11300     end if 
11310     let inl$(1)="1. Reprint the same check    "
11320     if ckoption=1 or ckoption=3 then 
11330       let inl$(2)="2. Continue with next check    "
11340     else 
11350       let inl$(2)="2. pr next check and Stop   "
11360     end if 
11370     if ckoption=1 or ckoption=3 then 
11380       let inl$(3)="3. Completed with checks"
11390     else 
11400       let inl$(3)="3. pr All remaining checks  "
11410     end if 
11420   end if  ! ~allign
11430 SCR_CKPRT7: ! 
11440   let fntos(sn$="ckprt-7")
11450   let respc=0
11460   let fnlbl(1,1,"",40,0)
11470   let fnlbl(1,1,"Print Options:",38,0)
11480   let fnopt(2,3,inl$(1),0)
11490   let resp$(respc+=1)="False"
11500   let fnopt(3,3,inl$(2),0)
11510   let resp$(respc+=1)="True" !  if ckoption=1 or ckoption=3 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
11520   let fnopt(4,3,inl$(3),0)
11530   if ckoption=2 then 
11540     let resp$(respc+=1)="True"
11550   else 
11560     let resp$(respc+=1)="False"
11570     if trim$(inl$(4))<>"" then 
11580       let fnopt(5,3,inl$(4),0)
11590       let resp$(respc+=1)="False"
11600     end if 
11610   end if 
11620   let fncmdset(2)
11630   let fnacs(sn$,0,mat resp$,ck)
11640   if (ck=5 or ck=99) and ckoption=1 then let fn_write_ck_hist_1 : goto MENU1
11650   if (ck=5 or cmdkey=99) then goto TRANS_TO_CK_HIST
11660   for j=1 to 4
11670     if resp$(j)(1:1)="T" then allign=j : goto L2300
11680   next j
11690 L2300: ! 
11700   if ckoption=1 and allign=2 then let fn_write_ck_hist_1 ! write regular check history if not a reprint
11710 ! L2310: ! 
11720   if ckoption=1 and allign=3 then let fn_write_ck_hist_1 !  write regular check history
11730   on allign goto ALIGN_REPR_SAME,ALIGN_PRINT_NEXT,ALIGN_COMPLETED,ALIGN_PRINT_NEXT none SCR_CKPRT7
11740 ! ______________________________________________________________________
11750 ALIGN_REPR_SAME: ! 
11760   if prenum=1 then 
11770     write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,g 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",1,dp,1
11780     ckn=ckn+1
11790     let tr$(1)=str$(ckn)
11800   end if 
11810   goto SUB_PRINT_CHECK
11820 ALIGN_COMPLETED: ! 
11830   if ckoption=3 and allign=3 then goto TRANS_TO_CK_HIST
11840 ALIGN_PRINT_NEXT: ! 
11850   if allign=4 and prenum=1 then 
11860     write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,g 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",0,dp,1
11870   end if 
11880   ckn=ckn+1
11890 L2420: ! 
11900   mat iv$=("") : mat de$=("")
11910   mat amt=(0) : mat de$=("")
11920   amt=arec=x=y=0
11930   let x=y=1
11940   let st1=0
11950   let holdvn$=vn$
11960   let tr$(3)=tr$(4)=tr$(5)=""
11970   let hiv$=""
11980   let tac=0
11990   if (ckoption=1 or ckoption=3) and allign=3 then goto MENU1
12000   return ! /r
12020 EOF_ROUTINE: ! r:
12030   if st1=1 then gosub SUB_PRINT_CHECK
12040   let fncloseprn
12050 ! if env$('client')="Washington Parrish" then let fnprocess(0)
12060   mat amt=(0) : mat de$=("") : mat iv$=("") : mat de$=("") : let x=y=1
12070   let st1=0 : let holdvn$=""
12080   goto MENU3
12090 ! /r
12100 MENU3: ! r: (reprint or transfer to history)
12110   let fntos(sn$="ckprt-4")
12120   let respc=0
12130   let fnlbl(1,1,"Reprint Options:",38)
12140   let item5$(1)="Reprint Checks"
12150   let item5$(2)="Transfer to Check History"
12160   let fncomboa("ckprt-cmb1",1,40,mat item5$,tt$)
12170   let resp$(respc+=1)=item5$(2)
12180   let fncmdset(41): let fnacs(sn$,0,mat resp$,ck)
12190   if resp$(1)=item5$(1) then let ti2=1 else let ti2=2
12200   allign=0
12210   on ti2 goto MENU4,TRANS_TO_CK_HIST none MENU3
12220 ! /r
12250 MENU4: ! r: (Reprint Options)
12260   let fntos(sn$="ckprt-reprint")
12270   let respc=0
12280   let fnlbl(1,1,"Reprint Options:",38)
12290   let item4$(1)="Reprint all checks"
12300   let item4$(2)="Begin with specific Payee"
12310   let fncomboa("ckprt-cmb2",1,40,mat item4$,tt$)
12320   let resp$(respc+=1)=item4$(1)
12330   let fnlbl(3,1,"Beginning payee number:",38)
12340   let fncombof("Paymstr",3,10,30,env$('Q')&"\CLmstr\paymstr.h"&env$('cno'),1,8,9,30,env$('Q')&"\CLmstr\Payidx1.h"&env$('cno'),0,pas, "Enter the beginning payee number if you wish to only reprint part of the checks")
12350   let resp$(respc+=1)=holdpayee$
12360   let fncmdset(2)
12370   let fnacs(sn$,0,mat resp$,ck)
12380   if ck=5 then goto XIT
12390   if resp$(1)=item4$(1) then let ti2=1 else let ti2=2
12400   begvn$=resp$(2)(1:8) ! beginning payee to start reprint
12410   on ti2 goto L3430,L3470
12420 L3430: ! REPRINT ALL
12430   restore #h_paytrans,key>="                    ": 
12440   let hiv$="" : let de$="" ! ???
12450   goto MAIN_QUESTIONS
12460 L3470: ! reprint beginning with specific payee
12470   restore #h_paytrans,key>=lpad$(rtrm$(begvn$),8)&"            ": nokey MENU4
12480   let hiv$="" : let de$=""
12490   goto MAIN_QUESTIONS
12500 ! /r
12510 TRANS_TO_CK_HIST: ! r: TRANSFER TO CHECK HISTORY
12520   let fn_write_history
12530   goto FINIS ! /r
12540 FINIS: ! r: COMPLETE
12550   let fn_close(1)
12560   let fn_close(trmstr2)
12570   let fn_close(tralloc)
12580   let fn_close(h_paytrans)
12590   let fn_close(ivpaid)
12600   let fn_close(bankmstr)
12610   let fn_close(h_paymstr1)
12620   let fn_close(company)
12630   if idx then let fn_index
12640   goto XIT ! /r
12650   def fn_close(h_closeme)
12660     close #h_closeme: ioerr CLOSE_IGNORE
12670 CLOSE_IGNORE: ! 
12680   fnend  ! fn_close
12700 XIT: let fnxit
12710 IGNORE: continue 
12730 CKOPTION1_CHECK_ENTRY: ! r:
12740   mat resp$=("")
12750 CKOPTION1_CHECK_ENTRY_2: ! 
12760   ck=fn_scr_check_entry
12770   if ck=5 then 
12780     goto XIT
12790   else if ck=20 or ck=21 then 
12800     goto ASSIGN_SCREENS
12810   else 
12820     goto STORE_GL_BREAKDOWNS
12830   end if 
12840 ! /r
12850 STORE_GL_BREAKDOWNS: ! r: store general ledger breakdowns
12860   let x=0 : let tac=0
12870   for j=1 to 146 step 5
12880     let x=x+3
12890     if x=33 then let x=35 ! skip check# and date (resp$(33)&34
12900     let in3$(j)=holdresp$(x)(1:3) ! gl$(1)
12910     let in3$(j+1)=holdresp$(x)(4:9) ! gl$(2)
12920     let in3$(j+2)=holdresp$(x)(10:12) ! gl$(3)
12930     let in3$(j+3)=holdresp$(x+1) ! amount
12940     let in3$(j+4)=holdresp$(x+2)(1:30) ! description  kj 081507
12950     let tac+=val(in3$(j+3))
12960   next j
12970   if ck=20 or ck=21 then 
12980     goto ASSIGN_SCREENS
12990   else 
13000     goto COMPLETED_WITH_SCREEN
13010   end if 
13020 ! /r
13030 ASSIGN_SCREENS: ! r: assign screen # based on more and back options
13040   if screen=0 then let screen=1
13050   if ck=20 and screen=1 then let screen=2 : goto L5070
13060   if ck=20 and screen=2 then let screen=3 : goto L5070
13070   if ck=20 and screen=3 then let screen=3 : goto L5070 ! shouldn't happen
13080   if ck=21 and screen=2 then let screen=1 : goto L5070
13090   if ck=21 and screen=1 then let screen=1 : goto L5070 ! shouldn't happen
13100   if ck=21 and screen=3 then let screen=2 : goto L5070
13110 L5070: for j=1 to 30
13120     if screen=1 then let x=j+2
13130     if screen=2 then let x=j+34
13140     if screen=3 then let x=j+64
13150     if int(j+2/3)=(j+2/3) then let resp$(j+2)=holdresp$(x) else let resp$(j+2)=holdresp$(x)
13160   next j
13170   goto CKOPTION1_CHECK_ENTRY_2 ! /r
13180 COMPLETED_WITH_SCREEN: ! r:
13190   let screen=0
13200   if ck<>2 then goto PRINT_REGULAR_CHECKS ! skip automatic allocation
13210   let fn_read_standard_breakdowns
13220   goto CKOPTION1_CHECK_ENTRY_2
13230 ! /r
13240 PRINT_REGULAR_CHECKS: ! r:
13250   let fn_cknum
13260   amt=arec=x=y=0
13270   mat amt=(0) : mat de$=("")
13280   if tac<>val(tr$(3)) then let fn_msg_allocations_off : goto CKOPTION1_CHECK_ENTRY_2 ! ALLOCATIONS NOT EQUAL
13290   for j=1 to 30 ! kj was 10
13300     if in3$(j*5)=hiv$ and rtrm$(hiv$)<>"" then goto L5460
13310     let y=y+1: if y>2 then let y=1
13320     if y=1 then let x=x+1
13330     let de$(x,y)=in3$(j*5)(1:13)
13340 L5460: ! 
13350     amt(x,y)=amt(x,y)+val(in3$(j*5-1))
13360     let hiv$=in3$(j*5)(1:15)
13370   next j
13380   amt=val(tr$(3)) : let vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
13390   gosub SUB_PRINT_CHECK
13400   mat holdresp$=("")
13410   goto CKOPTION1_CHECK_ENTRY
13420 ! /r
13430 ERTN: ! r:
13432   let fnerror(program$,err,line,act$,"xit")
13440   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
13450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
13460 ERTN_EXEC_ACT: execute act$ : goto ERTN
13470 ! /r
13480 REPRINT_CHECKS: ! r:
13490   let fntos(sn$="reprintnumber")
13500   let respc=0
13510   let fnlbl(1,1,"First Check Number to Reprint:",38,1)
13520   let fntxt(1,40,8,0,1,"30",0,"")
13530   let resp$(respc+=1)=str$(firstckn)
13540   let fnlbl(2,1,"Last Check Number to Reprint:",38,1)
13550   let fntxt(2,40,8,0,1,"30",0,"")
13560   let resp$(respc+=1)=str$(lastckn)
13570   if reprintckn>0 then let fnlbl(4,1,"Last Check Number Reprinted "&str$(reprintckn)&":",38,1)
13580   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
13590   if ck=5 then goto XIT
13600   let firstckn=ckn1=reprintckn=val(resp$(1))
13610   let lastckn=val(resp$(2)) : if lastckn=0 then let lastckn=firstckn
13620   if lastckn>0 and lastckn<firstckn then goto REPRINT_CHECKS ! smaller lastckn
13630 REPRINT_CHECK_LOOP_TOP: ! 
13640   check_ref$=cnvrt$("pic(ZZ)",bankcode)&str$(1)&cnvrt$("n 8",reprintckn)
13650   read #trmstr1,using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey L7780
13660 ! pr 'key=';check_ref$ : pause
13670   let prdmmddyy=val(tr$(2)) ! use old check date
13680   let vn$=lpad$(trim$(tr$(4)),8)
13690   amt=tr3 ! set amount for check
13700   mat amt=(0) : mat de$=("") : mat iv$=("") : let x=1: let y=0
13710   let st1=0 : let holdvn$="        ": amt=0
13720   let vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
13730   goto L7790
13740 L7780: ! 
13750   if firstckn=reprintckn then goto L7785
13760   if reprintckn>=lastckn then goto L7970 ! complete
13770   mat ml$(2)
13780   let ml$(1)="Could not locate check number "&str$(reprintckn)&" for bank number "&str$(bankcode)&"."
13790   let ml$(2)="This check will be skipped."
13800   let reprintckn+=1
13810   let fnmsgbox(mat ml$,resp$)
13820   goto REPRINT_CHECK_LOOP_TOP
13830 L7785: ! 
13840   mat ml$(2)
13850   let ml$(1)="Cannot locate the first check (number "&str$(reprintckn)&" for bank number "&str$(bankcode)&".)"
13860   let ml$(2)="You must choose another check number."
13870   let fnmsgbox(mat ml$,resp$)
13880   goto REPRINT_CHECKS
13890 L7790: ! 
13900   let key$=cnvrt$('pic(ZZ)',bankcode)&str$(1)&cnvrt$("n 8",reprintckn)
13910   restore #tralloc,key>=key$: nokey L7780
13920   do 
13930 READ_DETAILS: ! 
13940 ! 
13950     read #tralloc,using 'Form POS 1,N 2,N 1,n 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': transbankcode,tcode,transckn,gl$,alloc,allocde$,allocdate conv L7815,eof L7910
13960     goto L7820
13970 L7815: ! 
13980     reread #tralloc,using 'Form POS 1,c 2': x$ eof L7910 : goto READ_DETAILS
13990 L7820: ! 
14000     if transbankcode><bankcode or tcode<>1 or transckn<>reprintckn then goto L7910
14010     let y=y+1: if y>2 then let y=1
14020     if y=1 then let x+=1
14030     if x>15 then gosub SUB_PRINT_CHECK
14040     let iv$(x,y)=cnvrt$("pic(zzzzzz)",allocdate) ! IV$(1:12)
14050     let de$(x,y)=allocde$(1:13) ! this one for printing from unpaid file
14060     amt(x,y)=amt(x,y)+alloc
14070     let ivdate(x,y)=0
14080     let disamt(x,y)=0 ! already out of net check
14090   loop 
14100 L7910: ! 
14110   let fnopenprn
14120   ckn1=reprintckn: amt=tr3
14130   if scc$="CSS" then let fn_portion_check   : let fn_portion_stub(1) : let fn_portion_stub(2)
14140   if scc$="CSS" then let fn_portion_check   : let fn_portion_stub(1) : let fn_portion_stub(2)
14150   if scc$="SCS" then let fn_portion_stub(1) : let fn_portion_check   : let fn_portion_stub(2)
14160   if scc$="SSC" then let fn_portion_stub(1) : let fn_portion_stub(2) : let fn_portion_check
14170   if scc$="SCC" then let fn_portion_stub(1) : let fn_portion_check    : let fn_portion_check
14180   if lastckn>0 and reprintckn<lastckn then let reprintckn+=1 : pr #255: newpage : goto REPRINT_CHECK_LOOP_TOP
14190 L7970: ! 
14200   let fncloseprn
14210   if firstckn<>lastckn then goto XIT
14220   goto REPRINT_CHECKS ! /r
14230 def fn_get_coinfo
14240   open #company=15: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
14250   read #company,using 'Form POS 1,C 40,POS 150,2*N 1,N 2,POS 418,10*C 20,POS 668,10*C 12,POS 298,15*PD 4,POS 618,10*N 1,POS 406,N 1,POS 788,N 1',rec=1,release: cnam$,mat d,bankcode ,mat misc$,mat miscgl$,mat whgl,mat dedcode,prenum,port
14260   let method$="C" ! temporary kJ  ! Read #COMPANY,Using 'Form POS 789,c 1',Rec=1,Release: method$
14270   close #company: 
14280   for j=1 to 5
14290     let whgl$(j)=lpad$(str$(whgl(j,1)),3)&lpad$(str$(whgl(j,2)),6)&lpad$(str$(whgl(j,3)),3)
14300   next j
14310   let w1$=whgl$(1)
14320   let whgl$(1)=whgl$(2)
14330   let whgl$(2)=w1$
14340   ! 
14350   do 
14360     read #h_paytrans,using 'Form POS 63,N 10.2,N 1',release: upa,pcde eof EO_PAYTRANS_1
14370     if pcde=1 then let t1(2)+=upa else let t1(4)+=upa
14380     let t1(5)+=upa
14390   loop 
14400   EO_PAYTRANS_1: ! 
14410 fnend 
14910 def fn_scr_check_entry
14930   let fntos(sn$="ckprt-3")
14940   let respc=0
14950   let fnfra(1,1,6,87,"Check"," ")
14960   let fnlbl(1,1,env$('cnam'),40,2,0,1)
14970   let fnlbl(2,1,bn$,40,2,0,1)
14980   let fnlbl(3,55,"Amount:",10,1,0,1)
14990   let fntxt(3,67,12,0,1,"10",0,"",1)
15000   let resp$(respc+=1)=tr$(3)
15010   let fnlbl(5,1,"Payee:",8,1,0,1)
15020   let fncombof("Paymstr",5,10,30,env$('Q')&"\CLmstr\paymstr.h"&env$('cno'),1,8,9,30,env$('Q')&"\CLmstr\Payidx1.h"&env$('cno'),0,pas, "Enter the payee number or simply enter the payee name if no vendor record exits",1)
15030   let resp$(respc+=1)=holdpayee$
15040   let fnfra(9,1,12,96,"Breakdown Information"," ")
15050   let fnlbl(1,1,"General Ledger",30,0,0,2)
15052   let fnlbl(1,41,"Amount             Description",12,0,0,2)
15054   let fnlbl(1,56,"Description",30,0,0,2)
15060   for j=1 to 10
15070     let fnqgl(j+1,1,2,2)
15080     let resp$(respc+=1)=fnrgl$(resp$(respc))
15082     ! 
15090     let fntxt(j+1,41,12,0,1,"currency",0,"",2)
15100     let resp$(respc+=1)=resp$(respc)
15102     ! 
15110     let fntxt(j+1,56,30,0,0,"",0,"",2)
15120     let resp$(respc+=1)=resp$(respc)
15122     ! 
15130   next j
15140   if screen=2 or screen=3 then 
15150     let fnbutton(12,74,"Back",21,"Previous breakdown screen",1,4,2)
15160   end if 
15170   if screen=0 or screen=1 or screen=2 then 
15180     let fnbutton(12,82,"More",20,"Allows another screen of breakdowns",1,4,2)
15190   end if 
15200   let pas=1 ! don't redo combo boxes
15210   let fnlbl(1,45,"Check Number:",15,1,0,1)
15220   let fntxt(1,62,8,0,1,"30",0,"",1)
15230   let resp$(respc+=1)=str$(ckn)
15240   let fnlbl(3,30,"Check Date:",12,1,0,1)
15250   let fntxt(3,44,10,0,1,"3",0,"",1)
15260   let resp$(respc+=1)=str$(prd)
15270   let fnbutton(5,52,"Add Payee",50,"Click to add a new payee record",0,0,1)
15280   let fncmdkey("Print",1,1,0,"Prnt this check and advance to next check")
15290   let fncmdkey("&Allocate",2,0,0,"Automatically allocates the general ledger breakdown if payee record contains the breakdown information")
15300   let fncmdkey("&Complete",5,0,1,"Return to menu.")
15310   ! need a fncmdkey to change screens for the breakdowns  (screen 1,2 or 3)
15320   let fnacs(sn$,0,mat resp$,ck)
15340   if ck=5 then let screen=0 : goto SCE_XIT
15350   ! 
15360   for j=3 to 30, step 3
15370     let resp$(j)=fnagl$(resp$(j))
15380   next j
15390   ! 
15400   if ck=50 then let fn_payee_add : goto CKOPTION1_CHECK_ENTRY_2
15410   let tr$(3)=resp$(1) ! amount
15420   let vn$=tr$(4)=lpad$(rtrm$(resp$(2)(1:8)),8) ! payee number
15430   read #h_paymstr1,using "Form pos 1,c 8",key=vn$,release: vn$ nokey SCE_L4640
15440   let tr$(5)=resp$(2)(9:30) ! payee name
15450   goto SCE_L4650
15460   SCE_L4640: ! 
15470   let tr$(5)=resp$(2)(1:30)
15480   let vn$=tr$(4)="": b$(1)=tr$(5) ! payee name without vendor record
15490   SCE_L4650: ! 
15500   ckn=val(resp$(33)) ! ck number
15510   let holdpayee$=resp$(2)
15520   let prd=val(resp$(34)): ! date
15530   let prdmmddyy=val(resp$(34)(5:6))*10000+val(resp$(34)(7:8))*100+val(resp$(34)(3:4)) ! convert date back to mmddyy format
15540   let tr$(2)=cnvrt$("pic(######)",prdmmddyy)
15550   ! STORE_RESPONSES: ! hold all 94 possible responses in holdresp$
15560   let x=0
15570   if screen=0 then let screen=1
15580   for j=1 to 34
15590     if j=1 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! amount
15600     if j=2 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! vendor
15610     if j=33 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! checknumber
15620     if j=34 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! date
15630     if screen=1 then let x=j ! (3-32)
15640     if screen=2 then let x=j+32 ! (35-64)
15650     if screen=3 then let x=j+62 ! (65-94)
15660     if int(j+2/3)=(j+2/3) then 
15670       let holdresp$(x)=resp$(j)
15680     else 
15690       let holdresp$(x)=resp$(j) ! hold all general ledger breakdowns
15700     end if 
15710   SCE_L4820: ! 
15720   next j
15730   SCE_XIT: ! 
15740   let fn_scr_check_entry=ck
15750 fnend 
15760 def fn_read_standard_breakdowns ! pull standard gl breakdowns from payee file
15770   read #h_paymstr1,using "Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,X 6,C 12,C 30,C 50,C 12,C 20",key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey RSB_XIT
15780   mat holdresp$=("")
15790   restore #payeegl,key>=vn$: nokey RSB_EO_READSTGL
15800   let totalalloc=0
15810   for j=3 to 92 step 3
15820     if j=33 or j=34 then goto RSB_L5310 ! skip ck num and date  (resp$(33)&34)
15830   RSB_L5240: ! 
15840     read #payeegl,using "Form Pos 1,C 8,c 12,n 6.2,c 30",release: payeekey$,payeegl$,percent,gldesc$ eof RSB_EO_READSTGL
15850     if vn$<>payeekey$ then goto RSB_EO_READSTGL
15860     if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto RSB_L5310
15870     read #glmstr18,using 'Form POS 13,C 30',key=payeegl$,release: de$ nokey RSB_L5240
15880     let resp$(j)=payeegl$
15890     let resp$(j+1)=str$(round(val(tr$(3))*percent*.01,2))
15900     let totalalloc+=val(resp$(j+1))
15910     let resp$(j+2)=gldesc$ ! description
15920   RSB_L5310: ! 
15930   next j
15940   RSB_EO_READSTGL: ! 
15950   if val(tr$(3))<>totalalloc then 
15960     let resp$(4)=str$(val(resp$(4))+val(tr$(3))-totalalloc)
15970   end if 
15980   RSB_XIT: ! 
15990 fnend 
16000 def fn_write_history
16010   let holdvn$=""
16020   let hck=0
16030   let fn_close(h_paytrans:=4)
16040   open #h_paytrans: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
16050   WH_LOOP_TOP: ! 
16060   read #h_paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,ckpay,dp,gde eof WH_XIT
16070   if gde=1 then let gde=0 ! dont allow posting code of 1 from unpaid file
16080   if upa=0 then goto WH_L3910
16090   if dp=0 and ckpay=0 then goto WH_LOOP_TOP
16100   if bc=0 then bc=bankcode ! IF CODED FOR PAYMENT WITH NO BANK CODE, PAY FROM THE CURRENT BANK ACCOUNT
16110   if bc<>bankcode then goto WH_LOOP_TOP
16120   let iv$=rpad$(ltrm$(iv$),12)
16130   read #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8',key=vn$&iv$,release: vn$ nokey WH_L3650
16140   goto WH_L3660
16150   WH_L3650: ! 
16160   write #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8': vn$,iv$,dp,ckpay
16170   WH_L3660: ! 
16180   if vn$=holdvn$ and hck=ckpay then goto WH_L3770
16190   mat tr=(0)
16200   let totalupa=0
16210   let vn$=lpad$(rtrm$(vn$),8)
16220   read #h_paymstr1,using 'form pos 9,c 30',key=vn$,release: b$(1) nokey WH_L3740 ! PAYEE FILE
16230   if ltrm$(vn$)(1:2)="T-" then 
16240     delete #h_paymstr1,key=vn$: nokey ignore
16260   end if  ! ltrm$(vn$)(1:2)="T-"
16270   WH_L3740: ! 
16280   if holdvn$<>vn$ or (hck<>ck and hck>0) then 
16290     write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bc,1,ckpay,dp,upa,vn$,b$(1),0,0,1
16300   end if  ! holdvn$<>vn$ or (hck<>ck and hck>0)
16310   let holdvn$=vn$
16320   let hck=ck
16330   WH_L3770: ! 
16340   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WH_L3800
16350   bal=bal-upa
16360   rewrite #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckpay nokey ignore ! WH_L3800
16370   WH_L3800: ! form pos 1,n 2,n 1,g 8,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
16380   restore #h_unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_EO_UNPDALOC
16390   WH_L3820: ! 
16400   read #h_unpdaloc,using 'Form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$ eof WH_EO_UNPDALOC
16410   if trim$(allockey$(1:8))<>trim$(vn$) or trim$(allockey$(9:20))<>trim$(iv$) then goto WH_EO_UNPDALOC ! if ALLOCKEY$<>VN$&IV$ Then Goto 3690
16420   if sum(agl)=0 and aamt=0 then goto WH_L3820 ! don't allow zero allocations to write
16430   write #tralloc,using 'Form POS 1,N 2,N 1,G 8,G 3,G 6,G 3,PD 5.2,C 30,G 6,x 3,C 12,N 1': bc,1,ckpay,mat agl,aamt,ltrm$(rtrm$(iv$))&" "&ade$(1:17),up$(1),up$(3),gde
16440   ! Let HOLDVN$=VN$
16450   let hck=ckpay
16460   let totalupa+=aamt
16470   goto WH_L3820
16480   WH_EO_UNPDALOC: ! 
16490   rewrite #trmstr1,using 'Form POS 18,pd 10.2',key=lpad$(rtrm$(str$(bc)),2)&"1"&lpad$(rtrm$(str$(ckpay)),8): totalupa
16500   WH_L3910: ! 
16510   delete #h_paytrans: 
16520   do 
16530     delete #h_unpdaloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_L3940
16540   loop 
16550   WH_L3940: ! 
16560   goto WH_LOOP_TOP
16570   WH_XIT: ! 
16580 fnend 
16590 def fn_englishdollar(dolamt) ! returns eng$
16600   if ~setup_englishdollar then 
16610     let setup_englishdollar=1
16620     data One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Eleven,Twelve
16630     data Thirteen,Fourteen,Fifteen,Sixteen,Seventeen,Eighteen,Nineteen
16640     data Twenty,Thirty,Forty,Fifty,Sixty,Seventy,Eighty,Ninety
16650     read mat wording$
16660   end if  ! ~setup_englishdollar
16670   let dol=dolamt ! ENGLISH DOLLAR ROUTINE
16680   let n=64
16690   if dol<1000000 and dol>=0 then goto L2760
16700   let eng$="Value too big for editing or was less than zero"
16710   goto ENGLISHDOLLAR_XIT
16720   ! ______________________________________________________________________
16730   L2760: ! 
16740   let eng$="***"
16750   amount(1)=int(dol*100+.500000001)
16760   for a0=2 to 10
16770     amount(a0)=int(amount(a0-1)/10+.000000001)
16780   next a0
16790   for a0=1 to 10
16800     amount(a0)=amount(a0)-amount(a0+1)*10
16810   next a0
16820   if amount(11)+amount(10)+amount(9)=0 then goto L2880
16830   a0=9
16840   gosub ENGLISHDOLLAR_L3190
16850   let eng$=rtrm$(eng$)&" Million"
16860   L2880: if amount(8)+amount(7)+amount(6)=0 then goto L2920
16870   a0=6
16880   gosub ENGLISHDOLLAR_L3190
16890   let eng$=rtrm$(eng$)&" Thousand"
16900   L2920: ! 
16910   if amount(5)+amount(4)+amount(3)=0 then goto L2950
16920   a0=3
16930   gosub ENGLISHDOLLAR_L3190
16940   L2950: ! 
16950   if dol>=1 then goto L2970
16960   let eng$=rtrm$(eng$)&" Zero"
16970   L2970: ! 
16980   let eng$=rtrm$(eng$)&" Dollar"
16990   if dol<2 and dol>=1 then goto L3010
17000   let eng$=rtrm$(eng$)&"s"
17010   if len(rtrm$(eng$))>64 then goto L3010
17020   L3010: ! 
17030   let eng$=rtrm$(eng$)&" and"
17040   if amount(2)+amount(1)=0 then goto L3080
17050   amount(3)=0
17060   a0=1
17070   gosub ENGLISHDOLLAR_L3190
17080   goto L3090
17090   ! ______________________________________________________________________
17100   L3080: ! 
17110   let eng$=rtrm$(eng$)&" Zero"
17120   L3090: ! 
17130   let eng$=rtrm$(eng$)&" Cent"
17140   if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3120
17150   let eng$=rtrm$(eng$)&"s"
17160   L3120: ! 
17170   if len(rtrm$(eng$))<64 then goto L3170
17180   for j=1 to 9
17190     let n=65-j
17200     if eng$(n:n)=" " then goto L3170
17210   next j
17220   L3170: ! 
17230   goto ENGLISHDOLLAR_XIT
17240   ! ______________________________________________________________________
17250   ENGLISHDOLLAR_L3190: ! 
17260     if amount(a0+2)=0 then goto L3210
17270     let eng$=rtrm$(eng$)&" "&wording$(amount(a0+2))
17280     let eng$=rtrm$(eng$)&" Hundred"
17290     L3210: if amount(a0+1)=0 and amount(a0)=0 then goto ED_L3190_XIT
17300     if amount(a0+1)<2 then goto L3260
17310     let eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)+18)
17320     if amount(a0)=0 then goto ED_L3190_XIT
17330     amount(a0+1)=0
17340     L3260: let eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)*10+amount(a0))
17350     ED_L3190_XIT: ! 
17360   return  ! ENGLISHDOLLAR_L3190
17370   ENGLISHDOLLAR_XIT: ! 
17380 fnend 
17390 def fn_msg_allocations_off
17400   mat ml$(3)
17410   let ml$(1)="The net check ("&tr$(3)&") must agree with the total"
17420   let ml$(2)="allocations ("&str$(tac)&").  Correct the allocation"
17430   let ml$(3)="amounts or the net check to proceed."
17440   let fnmsgbox(mat ml$,resp$,'',16)
17450 fnend 
17460 def fn_checkdiscount ! check for any discounts
17470   if disamt=0 then goto DISCOUNTRETURN
17480   if ddate<prd then goto DISCOUNTRETURN ! already passed discount date
17490   let upa=upa-disamt
17500   rewrite #h_paytrans,using "form pos 63,n 10.2,pos 97,n 10.2",rec=lr4: upa,0 ! subtract discount, rewrite new unpaid invoice amount and zero discount amt
17510   restore #h_unpdaloc,key>=vn$&iv$: nokey DISCOUNTRETURN ! get the fund # from 1st g/l in the allocation file.
17520   read #h_unpdaloc,using 'Form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$
17530   if trim$(allockey$(1:8))=trim$(vn$) and trim$(allockey$(9:20))=trim$(iv$) then let fundkey$=cnvrt$("pic(ZZZ)",agl(1)) else let fundkey$="   "
17540   apgl$=discountgl$=""
17550   read #glcontrol, using "Form pos 52,c 12,pos 64,c 12",key=fundkey$: apgl$,discountgl$ nokey MSGBOX6
17560   L6410: ! 
17570   write #h_unpdaloc,using 'Form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,discountgl$,-disamt,"Discount=$"&str$(disamt)
17580   ! create an entry on the unpaid allocation file to record the discount
17590   if method$="A" and pcd>0 then write #h_unpdaloc,using 'Form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,apgl$,disamt,"Discount=$"&str$(disamt) ! create an entry in the unpaid allocation file to record the reduction in accounts payable if accrued and posted
17600   goto DISCOUNTRETURN
17610   MSGBOX6: ! 
17620   mat ml$(5)
17630   let ml$(1)="Invoice # "&trim$(iv$)&" on payee # "&trim$(vn$)&" quallifies for a discount of "&trim$(cnvrt$("pic($$$$$$.##)",disamt))&","
17640   let ml$(2)="but you have not entered the discount G/L # in the G/L control file."
17650   let ml$(3)="The discount will be taken, but the entry in check history will not"
17660   let ml$(4)="contain a G/L number.  Fix the GL # in the transaction file and place the "
17670   let ml$(5)="discount G/L #s in the G/L control file."
17680   let fnmsgbox(mat ml$,resp$,'',16)
17690   goto L6410
17700   DISCOUNTRETURN: ! 
17710 fnend 
17720 def fn_write_ck_hist_1 ! WRITE TRANSACTION FOR SINGLE CHECK ENTRY
17730   mat tr=(0)
17740   let tr$(1)=lpad$(str$(ckn),8)
17750   let tr$(4)=lpad$(rtrm$(tr$(4)),8)
17760   ! let k$=lpad$(str$(bankcode),2)&"1"&tr$(1)
17770   let tr$(1)=lpad$(str$(ckn),8)
17780   let tr3=val(tr$(3))
17790   write #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,tr$(1),prdmmddyy,tr3,tr$(4),tr$(5),0,clr,1
17800   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WCH1_AFTER_WRITE
17810   bn$=rtrm$(bn$)
17820   bal=bal-val(tr$(3)) conv ignore
17830   rewrite #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckn nokey WCH1_AFTER_WRITE
17840   WCH1_AFTER_WRITE: ! LET K$=LPAD$(RTRM$(STR$(BANKCODE)),2)&LPAD$(STR$(1),1)&LPAD$(TR$(1),8)
17850   for j=1 to 30
17860     if val(in3$(j*5-1))<>0 then 
17870       let gl$=""
17880       let gl$=cnvrt$("N 3",val(in3$(j*5-4)))&cnvrt$("N 6",val(in3$(j*5-3)))&cnvrt$("N 3",val(in3$(j*5-2)))
17890       alloc=val(in3$(j*5-1))
17900       let de$=in3$(j*5) ! de$=rtrm$(tr$(5)(1:17))&"-"&in3$(j*5)(1:12)
17910       let tr$(1)=lpad$(str$(ckn),8)
17920       write #tralloc,using 'Form POS 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': bankcode,1,tr$(1),gl$,alloc,de$,0,"",0
17930     end if 
17940   next j
17950   let tr$(2)
17960   mat tr$=("")
17970   mat in3$=("")
17980   if ltrm$(vn$)(1:2)="T-" then delete #h_paymstr1,key=vn$: nokey ignore 
18000 fnend 
18010 def fn_payee_add
18020   let vn$=tr$(4)
18030   mat pr$=("")
18040   mat desc$=("")
18050   mat gl$=("")
18060   contact$=email$=fax$=myact$=ss$=ph$=""
18070   let fnaddpayee
18080   let pas=0
18090 fnend 
18100 def fn_cknum ! CHECK FOR DUPLICATE CHECK NUMBERS
18110   CKNUM_TOP: ! CHECK FOR DUPLICATE CHECK NUMBERS
18120   let dk$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(ckn),8)
18130   read #trmstr1,using 'Form POS 4,C 8,G 6,pd 10.2,C 8,C 35',key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey CKNUM_XIT
18140   let dtr$(3)=str$(dtr3)
18150   SCR_CKPRT6: ! 
18160   let fntos(sn$="ckprt-6")
18170   let respc=0
18180   let fnlbl(1,1,"Check number "&str$(ckn)&" has been previously used.",45,1)
18190   let fnlbl(2,10," Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",val(dtr$(2))),45,0)
18200   let fnlbl(3,10," Amount: "&dtr$(3),45,0)
18210   let fnlbl(4,10," To: "&dtr$(5),45,0)
18220   let fnchk(6,48,"Delete the previous entry:",1)
18230   let resp$(respc+=1)="False"
18240   let fnlbl(8,1,"New check number (if applicable):",45,1)
18250   let fntxt(8,48,8,0,1,"30",0,"You will never enter the new check number if you are deleting the old check.")
18260   let resp$(respc+=1)=""
18270   ! ______________________________________________________________________
18280   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
18290   ckn2=val(resp$(2))
18300   if resp$(1)(1:1)="T" then goto CKNUM_DEL_PRV ! delete previous check
18310   if ckn2<=0 then 
18320     mat ml$(2)
18330     let ml$(1)="You must supply the new check number any time"
18340     let ml$(2)="you choose not to delete the old check."
18350     let fnmsgbox(mat ml$,resp$,'',16)
18360     goto SCR_CKPRT6
18370   end if 
18380   ckn=ckn2
18390   let tr$(1)=lpad$(str$(ckn2),8)
18400   goto CKNUM_TOP ! ***********************
18410   CKNUM_DEL_PRV: ! 
18420   bal=bal+val(dtr$(3))
18430   delete #trmstr1,key=dk$: 
18440   rewrite #bankmstr,using 'Form POS 45,PD 6.2',key=lpad$(str$(bankcode),2): bal nokey ignore
18460   restore #tralloc,key>=dk$: nokey CKNUM_XIT
18470   do 
18480     read #tralloc,using 'Form POS 1,c 11': trkey$ eof CKNUM_XIT
18490     if trkey$<>dk$ then goto CKNUM_XIT
18500     delete #tralloc,key=dk$: 
18510   loop 
18520   CKNUM_XIT: ! 
18530 fnend 
18540 def fn_index
18550   L4050: ! 
18560   open #31: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&env$('cno')&",NoShr",internal,outin,keyed ioerr L4070
18570   close #31: ! 
18580   goto L4080
18590   L4070: ! 
18600   mat ml$(2)
18610   let ml$(1)="You must get everyone out of the Unpaid Invoice File"
18620   let ml$(2)="before you can continue!  Press OK when ready."
18630   let fnmsgbox(mat ml$,resp$,'',16)
18640   goto L4050
18650   L4080: ! 
18660   let fn_close(h_unpdaloc)
18670   let fn_close(ivpaid)
18680   execute "Copy "&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')&" "&env$('temp')&"\X -D"
18690   execute "Free "&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')
18700   execute "ReName "&env$('temp')&"\X "&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')
18710   execute "Copy "&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')&" "&env$('temp')&"\X -D"
18720   execute "Free "&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')
18730   execute "ReName "&env$('temp')&"\X "&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')
18740   execute "Index "&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')&","&env$('Q')&"\CLmstr\UNPdIdx1.h"&env$('cno')&",1,20,Replace,DupKeys"
18750   execute "Index "&env$('Q')&"\CLmstr\unpdaloc.H"&env$('cno')&","&env$('Q')&"\CLmstr\Uaidx2.H"&env$('cno')&",1,20,Replace,DupKeys -n"
18760   execute "Index "&env$('Q')&"\CLmstr\unpdaloc.H"&env$('cno')&","&env$('Q')&"\CLmstr\Uaidx1.H"&env$('cno')&",9,12,Replace,DupKeys -n"
18770   execute "Index "&env$('Q')&"\CLmstr\IvPaid.h"&env$('cno')&","&env$('Q')&"\CLmstr\IVIndex.h"&env$('cno')&",1,20,ReOrg,DupKeys"
18780 fnend 
18790 ! 
18800 def fn_portion_stub(stubOnCheck)
18802   ! stubOnCheck - 1 or 2 to say if it is the first or second stub on a check.  Some formats care
18810   if env$('client')="Eldorado" then 
18840     let fn_portion_stub_eldorado
18850   else if env$('client')="Billings" then 
18860     let fn_portion_stub_billings(stubOnCheck)
18870   else if env$('client')="Divernon" then 
18880     let fn_portion_stub_divernon
18902   else 
18904     let fn_portion_stub_generic
18906     if env$('client')='Edison' and stubOnCheck=1 then
18908       pr #255: ''
18910       pr #255: ''
18912       pr #255: ''
18914       pr #255: ''
18916     end if
18930   end if 
18940 fnend 
18950 def fn_portion_stub_generic
18960   if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey L10970
18970   L10970: ! 
18980   pr #255: ""
18990   pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19000   pr #255: ""
19010   mat b$=(" ") : b$(1)=tr$(5)(1:30)
19020   if h_vf1=23 then let vp1=173 else let vp1=147
19030   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19050   pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19060   pr #255: "_______________________________________ _______________________________________"
19070   for j=1 to 15
19080     pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19090   next j
19100 fnend 
19110 def fn_portion_stub_billings(stubOnCheck)
19120   pr #255: ""
19130   if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19140   pr #255: ""
19150   pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19160   mat b$=(" ") : b$(1)=tr$(5)(1:30)
19170   if h_vf1=23 then let vp1=173 else let vp1=147
19180   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19190   pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19200   pr #255: "_______________________________________ _______________________________________"
19210   for j=1 to 15
19220     pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19230   next j
19250   if stubOnCheck=1 then ! it is the first stub on the check
19266     pr #255: ""
19268     pr #255: ""
19270     pr #255: ""
19280   end if 
19289 fnend 
19290 def fn_portion_stub_divernon
19300   pr #255: ""
19310   if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19320   pr #255: ""
19330   pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19340   pr #255: ""
19350   mat b$=(" ") : b$(1)=tr$(5)(1:30)
19360   if h_vf1=23 then let vp1=173 else let vp1=147
19370   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19380   pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19390   pr #255: "_______________________________________ _______________________________________"
19400   for j=1 to 15
19410     pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19420   next j
19430 fnend 
19440 def fn_portion_stub_eldorado
19450   if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19460   mat b$=(" ") : b$(1)=tr$(5)(1:30)
19470   if h_vf1=23 then let vp1=173 else let vp1=147
19480   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19490   if ckn1<>psb_ckn1_prior then ! it's the first stub on a check
19500     let psb_ckn1_prior=ckn1
19510     pr #255,using 'Form POS 74,N 6': ckn1 : pr ckn1
19520     pr #255: ""
19530     pr #255: ""
19540     F_PSE_ITEM: form pos 1,c 9,c 30,pic(zzzz,zzz.zzcr)
19550     for j=1 to 15
19560       pr #255,using F_PSE_ITEM: iv$(j,1)(1:9),de$(j,1),amt(j,1)
19570     next j
19580     for j=1 to 3
19590       pr #255,using F_PSE_ITEM: iv$(j,2)(1:9),de$(j,2),amt(j,2)
19600     next j
19610   else ! it's the second stub on a check
19620     pr #255: ""
19630     pr #255: "" ! pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19640     pr #255: ""
19650     pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19660     pr #255: "_______________________________________ _______________________________________"
19670     for j=1 to 15
19680       pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19690     next j
19700   end if  ! ckn1<>psb_ckn1_prior   /   else 
19710 fnend 
20050 ! 
40000 def fn_portion_check
40020   if env$('client')="ACS" and bankcode=2 then 
40040     let fn_portion_check_acs(amt)
40060   else if env$('client')="Ash Grove" then 
40080     let fn_portion_check_ashgrove(amt)
40140   else if env$('client')="Billings" then 
40160     let fn_portion_check_billings(amt)
40220   else if env$('client')="Cerro Gordo" then 
40240     let fn_portion_check_cerrogordo(amt)
40242   else if env$('client')="Cerro Gordo T" then 
40244     let fn_portion_check_generic(amt, 28,55)
40260   else if env$('client')="Divernon" then 
40280     let fn_portion_check_divernon(amt)
40290   else if env$('client')="Edison" then 
40292     fn_portion_check_edison(amt)
40300   else if env$('client')="Eldorado" then 
40320     let fn_portion_check_eldorado(amt)
40340   else if env$('client')="Kimberling" then 
40360     let fn_portion_check_kimber(amt)
40380   else if env$('client')="Lovington" then 
40400     fn_portion_check_generic(amt, 29)
40540   else 
40560     let fn_portion_check_generic(amt)
40580   end if 
40600 fnend 
40640 def fn_portion_check_generic(dolamt; length,posDate)
40660   ! r: gather information, etc
40680   mat b$=("")
40700   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
40720   if trim$(b$(2))="" then 
40740     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
40760   else if trim$(b$(3))="" then 
40780     b$(3)=b$(4) : b$(4)=""
40800   end if 
40820   let fn_englishdollar(dolamt)
40840   if dolamt=0 then let eng$='        *** V O I D ***'
40860   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
40880   ! /r
40900   if env$('client')="ACS" or env$('client')="Thomasboro" or env$('client')="Hope Welty" or env$('client')="Philo" or env$('client')="Divernon" then goto L1730 ! don't skip
40920   pr #255: "" ! line 1
40940   L1730: !
40960   skipline=9
40980   if prenum=2 then 
41000     pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1 ! line 2,3, 4
41020     skipline-=3
41040   end if
41060   pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
41080   pr #255: "" ! line 13
41082   let normal4=4
41084   if posDate then
41086     a=posDate
41088   else
41100     a=65
41140     if env$('client')="Bethany" then a=54
41160     if env$('client')="Thomasboro" or env$('client')="Unity" then a=55
41180     if env$('client')="Hope Welty" or env$('client')="Philo" then a=55
41200     if env$('client')="Monticello" or env$('client')="Edinburg" then a=55
41210   end if
41220   pr #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X normal4,C 18': prdmmddyy,ca$ ! line 14
41240   pr #255: ""
41260   pr #255: "" ! line 16
41280   if env$('client')="Cerro Gordo T"  then pr #255: ""
41300   for j=1 to 4
41320     pr #255,using "Form Pos 8,C 30": b$(j) ! lines 17-20
41340   next j
41360   if length=0 then ! do it the old way
41380     let skipline=6
41400     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
41420     ! if env$('client')="Washington Parrish" then let skipline=skipline-1
41440     if env$('client')="ACS" or env$('client')="Hope Welty" then let skipline=skipline+2
41460     if env$('client')="Philo" or env$('client')="Thomasboro" then let skipline=skipline+2
41480     ! if env$('client')="PiattCO" then let skipline=skipline+4
41500     for j=1 to skipline
41520       pr #255: ""
41540     next j
41560   else
41580     for lineItem=21 to length  ! default length is 27, i think
41600       pr #255: ''
41620     nex lineItem
41640   end if
41660 fnend 
41680 def fn_portion_check_ashgrove(dolamt)
41700   mat b$=("")
41720   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
41740   let fn_englishdollar(dolamt)
41760   let x=3 ! 1  add three lines after top stub
41780   for j=1 to x
41800     pr #255: ""
41820   next j
41840   if dolamt=0 then let eng$='        *** V O I D ***'
41860   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
41880   skipline=9
41900   if prenum=2 then pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
41920   if prenum=2 then let skipline=max(skipline-3,1)
41940   pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
41980   a=65
42000   let normal4=4
42020   pr #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X normal4,C 18': prdmmddyy,ca$
42060   pr #255: ""
42080   if trim$(b$(2))="" then 
42100     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
42120   else if trim$(b$(3))="" then 
42140     b$(3)=b$(4) : b$(4)=""
42160   end if 
42180   for j=1 to 4
42200     pr #255,using "Form Pos 8,C 30": b$(j)
42220   next j
42240   let skipline=6
42260   if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
42280   for j=1 to skipline+3
42300     pr #255: ""
42320   next j
42340 fnend 
43060 def fn_portion_check_eldorado(dolamt)
43080   mat b$=("")
43100   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey PCE_L1680
43120   if trim$(b$(2))="" then 
43140     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
43160   else if trim$(b$(3))="" then 
43180     b$(3)=b$(4) : b$(4)=""
43200   end if 
43220   PCE_L1680: ! 
43240   let fn_englishdollar(dolamt)
43260   if dolamt=0 then let eng$='        *** V O I D ***'
43280   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
43300   pr #255: ''
43320   pr #255: ''
43340   pr #255: ''
43360   if prenum=2 then 
43380     pr #255,using "form pos 74,n 8": ckn1
43400   else 
43420     pr #255: ''
43440   end if 
43460   pr #255: ''
43480   pr #255: ''
43500   pr #255: ''
43520   pr #255,using 'form pos 68,pic(zz/zz/zz)': prdmmddyy
43540   pr #255: ''
43560   pr #255: ''
43580   pr #255: ''
43600   pr #255,using 'Form Pos 9,C 80': eng$(1:n)
43620   pr #255,using 'Form Pos 9,C 70': eng$(n+1:128)
43640   pr #255,using 'Form POS 65,X 8,X 4,C 18': ca$
43660   pr #255: ''
43680   pr #255: ''
43700   for j=1 to 4
43720     pr #255,using "Form Pos 8,C 30": b$(j)
43740   next j
43760   pr #255: ''
43780   pr #255: ''
43800   pr #255: ''
43820   pr #255: ''
43840   pr #255: ''
43860   pr #255: ''
43880 fnend 
43900 def fn_portion_check_kimber(dolamt)
43920   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6480
43940   goto L6490
43960   L6480: mat b$=("")
43980   L6490: let fn_englishdollar(dolamt)
44000   let x=1
44020   for j=1 to x
44040     pr #255: ""
44060   next j
44080   if dolamt=0 then let eng$='        *** V O I D ***'
44100   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
44120   let skipline=9
44140   if prenum=2 then pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
44160   if prenum=2 then let skipline=max(skipline-3,1)
44180   pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
44200   pr #255: ""
44220   pr #255,using 'Form POS 60,PIC(ZZ/ZZ/ZZ),X 7,C 18': prdmmddyy,ca$
44240   pr #255: ""
44260   pr #255: ""
44280   if trim$(b$(2))="" then 
44300     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
44320   else if trim$(b$(3))="" then 
44340     b$(3)=b$(4) : b$(4)=""
44360   end if 
44380   for j=1 to 4
44400     pr #255,using "Form Pos 8,C 30": b$(j)
44420   next j
44440   let skipline=8
44460   if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
44480   for j=1 to skipline
44500     pr #255: ""
44520   next j
44540 fnend 
44560 def fn_portion_check_divernon(dolamt)
44580   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6690
44600   goto L6700
44620   L6690: mat b$=("")
44640   L6700: let fn_englishdollar(dolamt)
44660   let x=1
44680   for j=1 to x
44700     pr #255: ""
44720   next j
44740   if dolamt=0 then let eng$='        *** V O I D ***'
44760   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
44780   for j=1 to 9
44800     pr #255: " "
44820   next j
44840   a=62
44860   pr #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
44880   let skipline=2
44900   pr #255,using 'Form SKIP SKIPLINE,POS 4,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
44920   if trim$(b$(2))="" then 
44940     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
44960   else if trim$(b$(3))="" then 
44980     b$(3)=b$(4) : b$(4)=""
45000   end if 
45020   for j=1 to 4
45040     pr #255,using "Form Pos 8,C 30": b$(j)
45060   next j
45080   let skipline=6
45100   for j=1 to skipline
45120     pr #255: ""
45140   next j
45160 fnend 
47040 def fn_portion_check_acs(dolamt)
47060   mat b$=("")
47080   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
47100   if trim$(b$(2))="" then 
47120     b$(2)=b$(3) : b$(3)=b$(4) : b$(4)=""
47140   else if trim$(b$(3))="" then 
47160     b$(3)=b$(4) : b$(4)=""
47180   end if 
47200   let fn_englishdollar(dolamt)
47220   if dolamt=0 then let eng$='        *** V O I D ***'
47240   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
47260   pr #255: ""
47280   let skipline=9
47300   if prenum=2 then pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
47320   if prenum=2 then let skipline=max(skipline-3,1)
47340   pr #255,using "form skip 1, pos 80,pic(zz/zz/zz)": prdmmddyy
47360   pr #255: ""
47380   pr #255,using 'Form skip 1,POS 15,C 30,pos 73,c 18': b$(1),ca$
47400   pr #255: ""
47420   pr #255,using 'Form SKIP 1,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n),eng$(n+1:128)
47440   pr #255: ""
47460   for j=1 to 4
47480     pr #255,using "Form Pos 8,C 30": b$(j)
47500   next j
47520   let skipline=10
47540   if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
47560   for j=1 to skipline
47580     pr #255: ""
47600   next j
47620 fnend 
47640 def fn_portion_check_cerrogordo(dolamt)
47660   mat b$=("")
47680   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
47700   let fn_englishdollar(dolamt)
47720   let x=2
47740   for j=1 to x
47760     pr #255: ""
47780   next j
47800   if dolamt=0 then let eng$='        *** V O I D ***'
47820   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
47840   let skipline=8
47880   if prenum=2 then pr #255,using "form skip 3,pos 74,n 8,skip 1": ckn1
47900   if prenum=2 then let skipline=max(skipline-3,1)
47920   let skipline=skipline-2
47940   pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70,SKIP 1': eng$(1:n), eng$(n+1:128)
47960   pr #255: ""
47980   a=55
48020   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
48040   pr #255: ""
48060   pr #255: ""
48080   pr #255: ""
48100   pr #255: ""
48120   if trim$(b$(2))="" then 
48140     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
48160   else if trim$(b$(3))="" then 
48180     b$(3)=b$(4) : b$(4)=""
48200   end if 
48220   for j=1 to 4
48240     pr #255,using "Form Pos 8,C 30": b$(j)
48260   next j
48280   let skipline=6
48300   if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
48320   for j=1 to skipline
48340     pr #255: ""
48360   next j
48380 fnend 
49100 def fn_portion_check_billings(dolamt)
49120   mat b$=("")
49140   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
49160   if trim$(b$(2))="" then 
49180     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
49200   else if trim$(b$(3))="" then 
49220     b$(3)=b$(4) : b$(4)=""
49240   end if 
49260   let fn_englishdollar(dolamt)
49280   if dolamt=0 then let eng$='        *** V O I D ***'
49300   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
49320 ! 
49340   pr #255: ""
49360   pr #255,using 'form pos 42,c 38': "Void After 60 Days"
49380   pr #255: ""
49400   pr #255: ""
49420   pr #255: ""
49440   if trim$(eng$(n+1:128))='' then 
49460     pr #255: ""
49480     pr #255,using 'Form POS 9,C 80': eng$(1:n)
49500   else 
49520     pr #255,using 'Form POS 9,C 80': eng$(1:n)
49540     pr #255,using 'Form POS 9,C 70': eng$(n+1:128)
49560   end if 
49580   pr #255,using 'Form POS 59,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
49600   pr #255: ""
49620   pr #255: ""
49640   pr #255: ""
49660   pr #255,using "Form Pos 8,C 30": b$(1)
49680   pr #255,using "Form Pos 8,C 30": b$(2)
49700   pr #255,using "Form Pos 8,C 30": b$(3)
49720   pr #255,using "Form Pos 8,C 30": b$(4)
49780     pr #255: ""
49800     pr #255: ""
49820     pr #255: ""
49840     pr #255: ""
49860     pr #255: ""
49880     pr #255: ""
49900     pr #255: ""
49920     pr #255: ""
49940     pr #255: ""
49960     pr #255: ""
49990 fnend 
50000 def fn_portion_check_edison(dolamt)
50020   mat b$=("")
50040   read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
50060   if trim$(b$(2))="" then 
50080     b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
50100   else if trim$(b$(3))="" then 
50120     b$(3)=b$(4) : b$(4)=""
50140   end if 
50160   let fn_englishdollar(dolamt)
50180   if dolamt=0 then let eng$='        *** V O I D ***'
50200   if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
50220   ! 
50240   pr #255: ''
50260   pr #255: ''
50280   pr #255: ''
50300   pr #255: ''
50320   pr #255: ''
50340   pr #255,using 'Form POS 74,PIC(ZZ/ZZ/ZZ),pos 82,N 8': prdmmddyy,ckn1
50360   pr #255: ''
50380   pr #255: ''
50400   if trim$(eng$(n+1:128))='' then 
50420     pr #255: ""
50440     pr #255,using 'Form POS 9,C 80': eng$(1:n)
50460   else 
50480     pr #255,using 'Form POS 9,C 80': eng$(1:n)
50500     pr #255,using 'Form POS 9,C 70': eng$(n+1:128)
50520   end if 
50540   pr #255,using 'Form POS 79,C 18': ca$   ! line 11
50560   pr #255: ""
50580   pr #255,using "Form Pos 8,C 30": b$(1)
50600   pr #255,using "Form Pos 8,C 30": b$(2)
50620   pr #255,using "Form Pos 8,C 30": b$(3)
50640   pr #255,using "Form Pos 8,C 30": b$(4)
50660   pr #255: ''
50680   pr #255: ''
50700   pr #255: ''
50720   pr #255: ''
50740   pr #255: ''
50760   pr #255: ''
50780   pr #255: ''
50800   pr #255: ''
50820   pr #255: ''
50840 fnend 
64000 def fn_scr_main_questions
64020   if ~smq_setup then
64040     smq_setup=1
64060     dim layoutOption$(4)*18
64080     layoutOption$(1)="Stub, Check, Stub"  : scc$(1)="SCS"
64100     layoutOption$(2)="Check, Stub, Stub"  : scc$(2)="CSS"
64120     layoutOption$(3)="Stub, Stub, Check"  : scc$(3)="SSC"
64140     layoutOption$(4)="Stub, Check, Check" : scc$(4)="SCC"
64160     fncreg_read('Check Layout Option',layoutOptionSelected$, layoutOption$(1))
64180   end if
64200   let fntos(sn$="ckprt1a")
64220   let respc=0
64240   let fnlbl(1,1,"Method of Printing checks:",38,1)
64260   let fnopt(1,40,"Enter and pr Checks",0)
64280   if ckoption<=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
64300   let fnopt(2,40,"Print Checks for Selected Invoices",0)
64320   if ckoption=2 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
64340   let fnopt(3,40,"Reprint from Check History",0)
64360   if ckoption=3 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
64380   let fnlbl(5,1,"Date of Checks:",38,1)
64400   let fntxt(5,40,10,0,1,"3",0,"")
64420   let resp$(respc+=1)=date$("ccYYMMDD")
64440   let fnlbl(6,1,"Beginning check number:",38,1)
64460   let fntxt(6,40,8,0,1,"30",0,"Next available check #. If reprinting checks from history, this check # is not applicable.")
64480   let resp$(respc+=1)=str$(ckn)
64500   let fnlbl(7,1,"Bank Account:",38,1)
64520   let fncombof("Bankmstr",7,40,20,env$('Q')&"\CLmstr\bankmstr.h"&env$('cno'),1,2,3,15,env$('Q')&"\CLmstr\Bankidx1.h"&env$('cno'),1,0, "Select bank account for printing")
64540   let resp$(respc+=1)=str$(bankcode)
64560   let fnlbl(8,1,"Check Format:",38,1)
64580   let fncomboa("ckprt-2",8,40,mat layoutOption$)
64600   let resp$(respc+=1)=layoutOptionSelected$
64620   !   if env$('client')="Washington Parrish" then let resp$(respc)=layoutOption$(4)
64640   if env$('client')="Billings" or (env$('client')="ACS"and bankcode=2) then let resp$(respc)=layoutOption$(2)
64660   ! need button to show totals
64680   let fncmdset(2)
64700   let fnacs(sn$,0,mat resp$,ck)
64720   if ck<>5 then 
64740     for j=1 to 3
64760       if resp$(j)='True' then let ti1=j : ckoption=j
64780     next j
64800     let prd=val(resp$(4)) ! date of checks
64820     let prdmmddyy=val(resp$(4)(5:6))*10000+val(resp$(4)(7:8))*100+val(resp$(4)(3:4)) ! convert date back to mmddyy format
64840     ckn=val(resp$(5)) ! beginning ck number
64860     bankcode=val(resp$(6)(1:3))
64880     layoutOptionSelected$=resp$(7)
64900     for j=1 to 4
64920       if trim$(layoutOptionSelected$)=trim$(layoutOption$(j)) then let scc$=scc$(j)
64940     next j
64960     fncreg_write('Check Layout Option',layoutOptionSelected$)
64980   end if  ! ck<>5
65000   let fn_scr_main_questions=ck
65020 fnend 