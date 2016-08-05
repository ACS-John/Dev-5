10000 ! 
10010 ! print checks
10020 ! ______________________________________________________________________
10030   library 'R:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fnclient$,fntos,fnlbl,fntxt,fncomboa,fnchk,fncmdset,fnacs,fncombof,fnfra,fnmsgbox,fnbutton,fnopt,fncmdkey,fnaddpayee,fnqgl,fnagl$,fnrgl$,fnprocess
10040   on error goto ERTN
10050 ! ______________________________________________________________________
10060   dim vn$*8,holdvn$*8,up$(4),amt(15,3),iv$(15,3),de$(15,3)*13,ivdate(15,3)
10070   dim cnam$*40,tr(2),misc$(10)*20,miscgl$(10)*12
10080   dim de$*50,lcn$*8,whgl$(5)*12,gl$*12,allocde$*30
10090   dim whgl(5,3),in3$(150)*30,bn$*30,b$(4)*30
10100   dim ade$*30,dedcode(10),agl(3),de$*50
10110   dim t1(5),arec(100),d(2)
10120   dim pr$(4)*30,myact$*20,disamt(15,3)
10130   dim item2$(4)*18,resp$(50)*50,item4$(2)*35,item5$(2)*35
10140   dim holdpayee$*50,holdresp$(94)*50,contact$*30,email$*50,fax$*12
10150   dim inl$(4)*50,ml$(5)*90
10160   dim allockey$*20
10170   dim eng$*128,wording$(27)*9,amount(11)
10180   dim tr$(5)*35,sn$*30,dtr$(5)*35,payeegl$*12,gldesc$*30
10190 ! ______________________________________________________________________
10200   let fntop(program$, cap$="Print Checks")
10210   let fncno(cno)
10220   let client$=fnclient$
10230   let prd=val(date$(4:5)&date$(7:8)&date$(1:2))
10240   open #bankmstr=12: "Name=Q:\CLmstr\BankMstr.h"&str$(cno)&",KFName=Q:\CLmstr\BankIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
10250   open #h_paymstr1=13: "Name=Q:\CLmstr\PayMstr.h"&str$(cno)&",KFName=Q:\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
10260   open #paymstr2=14: "Name=Q:\CLmstr\PayMstr.h"&str$(cno)&",KFName=Q:\CLmstr\PayIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
10270   open #trmstr1=1: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
10280   open #trmstr2=2: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
10290   open #tralloc=3: "Name=Q:\CLmstr\TrAlloc.h"&str$(cno)&",KFName=Q:\CLmstr\tralloc-idx.h"&str$(cno)&",Shr",internal,outin,keyed 
10300   open #h_paytrans=4: "Name=Q:\CLmstr\PayTrans.h"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
10310   open #h_unpdaloc=7: "Name=Q:\CLmstr\UnPdAloc.h"&str$(cno)&",KFName=Q:\CLmstr\uaidx2.h"&str$(cno)&",Shr",internal,outin,keyed 
10320   open #glmstr18=18: "Name=Q:\CLmstr\GLmstr.H"&str$(cno)&",KFName=Q:\CLmstr\GLIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
10330   open #glcontrol=19: "Name=Q:\CLmstr\Fundmstr.h"&str$(cno)&",KFName=Q:\CLmstr\Fundidx1.h"&str$(cno)&",Shr",internal,outin,keyed 
10340   open #ivpaid=6: "Name=Q:\CLmstr\IvPaid.h"&str$(cno)&",KFName=Q:\CLmstr\IVIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
10350   open #payeegl=17: "Name=Q:\CLmstr\payeeGLBreakdown.h"&str$(cno)&",KFName=Q:\CLmstr\Payeeglbkdidx.h"&str$(cno)&",Shr",internal,outin,keyed 
10360   let fn_get_coinfo
10370 MENU1: ! 
10380   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
10390   let t1(1)=bal
10400   let upi=t1(5)
10410   let t1(3)=t1(1)-t1(2)
10420   let ckn=val(lcn$)+1 conv ignore
10430   let bn$=rtrm$(bn$)
10440 MAIN_QUESTIONS: ! 
10450   if fn_scr_main_questions=5 then goto XIT
10460 ! 
10470   let tac=0
10480   if ti1=3 and pri=1 then let h_vf1=23 else let h_vf1=h_paymstr1
10490   if ti1=1 then let h_vf1=13
10500   let allign=0
10510   if ti1=3 then goto REPRINT_CHECKS
10520   open #company=15: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative 
10530   rewrite #company,using 'Form POS 152,N 2',rec=1: bankcode
10540   close #company: 
10550   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
10560   mat in3$=("")
10570   let bn$=rtrm$(bn$)
10580   if ti1=1 or ti1=3 then goto CKOPTION1_CHECK_ENTRY
10590   restore #h_paytrans,key>=lpad$(rtrm$(begvn$),8)&"            ": nokey MENU1
10600   let amt=arec=x=y=0
10610   mat amt=(0) : mat de$=("")
10620 READ_4: ! 
10630   read #h_paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,n 1,n 6,n 10.2,n 8',release: vn$,iv$,mat up$,upa,pcde,bc,ck$,dp,gde,pdate,disamt,ddate eof EOF_ROUTINE
10640   let vn$=lpad$(rtrm$(vn$),8)
10650   if rtrm$(vn$)="" then goto READ_4
10660   if pcde=0 then goto READ_4
10670   if bc=0 then let bc=bankcode ! if coded for payment with no bank code, pay from the  current bank account
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
10810   let amt(x,y)=amt(x,y)+upa
10820   let ivdate(x,y)=dp
10830   let disamt(x,y)=disamt
10840   let arec+=1
10850   let arec(arec)=lr4
10860   let holdvn$=vn$
10870   let hiv$=iv$
10880   let amt=sum(amt)
10890   let st1=1
10900   goto READ_4
10910 ! ______________________________________________________________________
10920 SUB_PRINT_CHECK: ! 
10930   let fn_cknum
10940 ! if client$="Washington Parrish" then let fnprocess(1) ! skip Atlantis screen
10950   let fnopenprn(cp,42,220,process)
10960   let rtf$="Y" ! if uprc$(file$(255)(len(file$(255))-2:len(file$(255))))="RTF" then let rtf$="Y" else let rtf$="N"
10970   let ckn1=ckn
10980   on ckoption goto L1360,L1360 none L1390
10990 L1360: ! 
11000   if amt<=0 then goto L2420
11010   if client$="Brazeal" then let fn_portion_stub : let fn_portion_check : goto SPC_XIT
11020   if scc$="CSS" then let fn_portion_check : let fn_portion_stub : let fn_portion_stub
11030 L1390: ! 
11040   if scc$="SCS" then let fn_portion_stub : let fn_portion_check : let fn_portion_stub
11050   if client$="PiattCO" then let fn_portion_stub : let fn_portion_check : let fn_portion_stub
11060   if scc$="SSC" then let fn_portion_stub : let fn_portion_stub : let fn_portion_check
11070   if scc$="SCC" then let fn_portion_stub : let fn_portion_check : let fn_portion_check
11080 SPC_XIT: ! 
11090   gosub UPDATEINVOICE
11100   return 
11110 ! ______________________________________________________________________
11120 UPDATEINVOICE: ! 
11130   for j=1 to arec
11140     rewrite #h_paytrans,using 'Form POS 76,N 8,N 6',rec=arec(j): ckn,prdmmddyy
11150   next j
11160   let idx=1
11170   if client$="Brazeal" then let fnprocess(0) : let allign=2: goto L2310 ! no newpage
11180   if allign=3 then print #255: newpage : goto ALIGN_COMPLETED
11190   print #255: newpage
11200   let fncloseprn
11210 ! if client$="Washington Parrish" then let fnprocess(0)
11220   let holdpayee$=""
11230   if ti1=1 then let ckoption=1 : let allign=2 : goto L2300 ! skip the continue routine when entering and printing checks
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
11350       let inl$(2)="2. Print next check and Stop   "
11360     end if 
11370     if ckoption=1 or ckoption=3 then 
11380       let inl$(3)="3. Completed with checks"
11390     else 
11400       let inl$(3)="3. Print All remaining checks  "
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
11670     if resp$(j)(1:1)="T" then let allign=j : goto L2300
11680   next j
11690 L2300: ! 
11700   if ckoption=1 and allign=2 then let fn_write_ck_hist_1 ! write regular check history if not a reprint
11710 L2310: ! 
11720   if ckoption=1 and allign=3 then let fn_write_ck_hist_1 !  write regular check history
11730   on allign goto ALIGN_REPR_SAME,ALIGN_PRINT_NEXT,ALIGN_COMPLETED,ALIGN_PRINT_NEXT none SCR_CKPRT7
11740 ! ______________________________________________________________________
11750 ALIGN_REPR_SAME: ! 
11760   if prenum=1 then 
11770     write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,g 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",1,dp,1
11780     let ckn=ckn+1
11790     let tr$(1)=str$(ckn)
11800   end if 
11810   goto SUB_PRINT_CHECK
11820 ALIGN_COMPLETED: ! 
11830   if ckoption=3 and allign=3 then goto TRANS_TO_CK_HIST
11840 ALIGN_PRINT_NEXT: ! 
11850   if allign=4 and prenum=1 then 
11860     write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,g 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",0,dp,1
11870   end if 
11880   let ckn=ckn+1
11890 L2420: ! 
11900   mat iv$=("") : mat de$=("")
11910   mat amt=(0) : mat de$=("")
11920   let amt=arec=x=y=0
11930   let x=y=1
11940   let st1=0
11950   let holdvn$=vn$
11960   let tr$(3)=tr$(4)=tr$(5)=""
11970   let hiv$=""
11980   let tac=0
11990   if (ckoption=1 or ckoption=3) and allign=3 then goto MENU1
12000   return 
12010 ! ______________________________________________________________________
12020 EOF_ROUTINE: ! 
12030   if st1=1 then gosub SUB_PRINT_CHECK
12040   let fncloseprn
12050 ! if client$="Washington Parrish" then let fnprocess(0)
12060   mat amt=(0) : mat de$=("") : mat iv$=("") : mat de$=("") : let x=y=1
12070   let st1=0 : let holdvn$=""
12080   goto MENU3
12090 ! ______________________________________________________________________
12100 MENU3: ! 
12110   let fntos(sn$="ckprt-4")
12120   let respc=0
12130   let fnlbl(1,1,"Reprint Options:",38)
12140   let item5$(1)="Reprint Checks"
12150   let item5$(2)="Transfer to Check History"
12160   let fncomboa("ckprt-cmb1",1,40,mat item5$,tt$)
12170   let resp$(respc+=1)=item5$(2)
12180   let fncmdset(41): let fnacs(sn$,0,mat resp$,ck)
12190   if resp$(1)=item5$(1) then let ti2=1 else let ti2=2
12200   let allign=0
12210   on ti2 goto L3290,TRANS_TO_CK_HIST none MENU3
12220 ! ______________________________________________________________________
12230 L3290: ! REPRINT CHECKS
12240 ! 
12250 MENU4: ! 
12260   let fntos(sn$="ckprt-reprint")
12270   let respc=0
12280   let fnlbl(1,1,"Reprint Options:",38)
12290   let item4$(1)="Reprint all checks"
12300   let item4$(2)="Begin with specific Payee"
12310   let fncomboa("ckprt-cmb2",1,40,mat item4$,tt$)
12320   let resp$(respc+=1)=item4$(1)
12330   let fnlbl(3,1,"Beginning payee number:",38)
12340   let fncombof("Paymstr",3,10,30,"Q:\CLmstr\paymstr.h"&str$(cno),1,8,9,30,"Q:\CLmstr\Payidx1.h"&str$(cno),0,pas, "Enter the beginning payee number if you wish to only reprint part of the checks")
12350   let resp$(respc+=1)=holdpayee$
12360   let fncmdset(2)
12370   let fnacs(sn$,0,mat resp$,ck)
12380   if ck=5 then goto XIT
12390   if resp$(1)=item4$(1) then let ti2=1 else let ti2=2
12400   let begvn$=resp$(2)(1:8) ! beginning payee to start reprint
12410   on ti2 goto L3430,L3470
12420 L3430: ! REPRINT ALL
12430   restore #h_paytrans,key>="                    ": 
12440   let hiv$="" : let de$="" ! ???
12450   goto MAIN_QUESTIONS
12460 L3470: ! reprint beginning with specific payee
12470   restore #h_paytrans,key>=lpad$(rtrm$(begvn$),8)&"            ": nokey MENU4
12480   let hiv$="" : let de$=""
12490   goto MAIN_QUESTIONS
12500 ! 
12510 TRANS_TO_CK_HIST: ! TRANSFER TO CHECK HISTORY
12520   let fn_write_history
12530   goto FINIS
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
12760   let ck=fn_scr_check_entry
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
13260   let amt=arec=x=y=0
13270   mat amt=(0) : mat de$=("")
13280   if tac<>val(tr$(3)) then let fn_msg_allocations_off : goto CKOPTION1_CHECK_ENTRY_2 ! ALLOCATIONS NOT EQUAL
13290   for j=1 to 30 ! kj was 10
13300     if in3$(j*5)=hiv$ and rtrm$(hiv$)<>"" then goto L5460
13310     let y=y+1: if y>2 then let y=1
13320     if y=1 then let x=x+1
13330     let de$(x,y)=in3$(j*5)(1:13)
13340 L5460: ! 
13350     let amt(x,y)=amt(x,y)+val(in3$(j*5-1))
13360     let hiv$=in3$(j*5)(1:15)
13370   next j
13380   let amt=val(tr$(3)) : let vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
13390   gosub SUB_PRINT_CHECK
13400   mat holdresp$=("")
13410   goto CKOPTION1_CHECK_ENTRY
13420 ! /r
13430 ERTN: ! r:
13432   let fnerror(cap$,err,line,act$,"xit")
13440   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
13450   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
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
13640   let check_ref$=cnvrt$("pic(ZZ)",bankcode)&str$(1)&cnvrt$("n 8",reprintckn)
13650   read #trmstr1,using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey L7780
13660 ! pr 'key=';check_ref$ : pause
13670   let prdmmddyy=val(tr$(2)) ! use old check date
13680   let vn$=lpad$(trim$(tr$(4)),8)
13690   let amt=tr3 ! set amount for check
13700   mat amt=(0) : mat de$=("") : mat iv$=("") : let x=1: let y=0
13710   let st1=0 : let holdvn$="        ": let amt=0
13720   let vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
13730   goto L7790
13740 L7780: ! 
13750   if firstckn=reprintckn then goto L7785
13760   if reprintckn>=lastckn then goto L7970 ! complete
13770   mat ml$(2)
13780   let ml$(1)="Could not locate check number "&str$(reprintckn)&" for bank number "&str$(bankcode)&"."
13790   let ml$(2)="This check will be skipped."
13800   let reprintckn+=1
13810   let fnmsgbox(mat ml$,resp$,cap$,0)
13820   goto REPRINT_CHECK_LOOP_TOP
13830 L7785: ! 
13840   mat ml$(2)
13850   let ml$(1)="Cannot locate the first check (number "&str$(reprintckn)&" for bank number "&str$(bankcode)&".)"
13860   let ml$(2)="You must choose another check number."
13870   let fnmsgbox(mat ml$,resp$,cap$,0)
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
14060     let amt(x,y)=amt(x,y)+alloc
14070     let ivdate(x,y)=0
14080     let disamt(x,y)=0 ! already out of net check
14090   loop 
14100 L7910: ! 
14110   let fnopenprn
14120   let ckn1=reprintckn: let amt=tr3
14130   if scc$="CSS" then let fn_portion_check : let fn_portion_stub : let fn_portion_stub
14140   if scc$="CSS" then let fn_portion_check : let fn_portion_stub : let fn_portion_stub
14150   if scc$="SCS" then let fn_portion_stub : let fn_portion_check : let fn_portion_stub
14160   if scc$="SSC" then let fn_portion_stub : let fn_portion_stub : let fn_portion_check
14170   if scc$="SCC" then let fn_portion_stub : let fn_portion_check : let fn_portion_check
14180   if lastckn>0 and reprintckn<lastckn then let reprintckn+=1 : print #255: newpage : goto REPRINT_CHECK_LOOP_TOP
14190 L7970: ! 
14200   let fncloseprn
14210   if firstckn<>lastckn then goto XIT
14220   goto REPRINT_CHECKS ! /r
14230   def fn_get_coinfo
14240     open #company=15: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative 
14250     read #company,using 'Form POS 1,C 40,POS 150,2*N 1,N 2,POS 418,10*C 20,POS 668,10*C 12,POS 298,15*PD 4,POS 618,10*N 1,POS 406,N 1,POS 788,N 1',rec=1,release: cnam$,mat d,bankcode ,mat misc$,mat miscgl$,mat whgl,mat dedcode,prenum,port
14260     let method$="C" ! temporary kJ  ! Read #COMPANY,Using 'Form POS 789,c 1',Rec=1,Release: method$
14270     close #company: 
14280     for j=1 to 5
14290       let whgl$(j)=lpad$(str$(whgl(j,1)),3)&lpad$(str$(whgl(j,2)),6)&lpad$(str$(whgl(j,3)),3)
14300     next j
14310     let w1$=whgl$(1)
14320     let whgl$(1)=whgl$(2)
14330     let whgl$(2)=w1$
14340 ! 
14350     do 
14360       read #h_paytrans,using 'Form POS 63,N 10.2,N 1',release: upa,pcde eof EO_PAYTRANS_1
14370       if pcde=1 then let t1(2)+=upa else let t1(4)+=upa
14380       let t1(5)+=upa
14390     loop 
14400 EO_PAYTRANS_1: ! 
14410   fnend 
14420   def fn_scr_main_questions
14430     let fntos(sn$="ckprt1a")
14440     let respc=0
14450     let fnlbl(1,1,"Method of Printing checks:",38,1)
14460     let fnopt(1,40,"Enter and Print Checks",0)
14470     if ckoption<=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
14480     let fnopt(2,40,"Print Checks for Selected Invoices",0)
14490     if ckoption=2 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
14500     let fnopt(3,40,"Reprint from Check History",0)
14510     if ckoption=3 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
14520     let fnlbl(5,1,"Date of Checks:",38,1)
14530     let fntxt(5,40,10,0,1,"3",0,"")
14540     let resp$(respc+=1)=date$("ccYYMMDD")
14550     let fnlbl(6,1,"Beginning check number:",38,1)
14560     let fntxt(6,40,8,0,1,"30",0,"Next available check #. If reprinting checks from history, this check # is not applicable.")
14570     let resp$(respc+=1)=str$(ckn)
14580     let fnlbl(7,1,"Bank Account:",38,1)
14590     let fncombof("Bankmstr",7,40,20,"Q:\CLmstr\bankmstr.h"&str$(cno),1,2,3,15,"Q:\CLmstr\Bankidx1.h"&str$(cno),1,0, "Select bank account for printing")
14600     let resp$(respc+=1)=str$(bankcode)
14610     let fnlbl(8,1,"Check Format:",38,1)
14620     let item2$(1)="Stub, Check, Stub"
14630     let item2$(2)="Check, Stub, Stub"
14640     let item2$(3)="Stub, Stub, Check"
14650     let item2$(4)="Stub, Check, Check"
14660     let fncomboa("ckprt-2",8,40,mat item2$)
14670     let resp$(respc+=1)=item2$(1)
14675     if client$="Miller Hardware" then let resp$(respc)=item2$(2)
14680 !   if client$="Washington Parrish" then let resp$(respc)=item2$(4)
14690     if client$="Billings" or (client$="ACS"and bankcode=2) then let resp$(respc)=item2$(2)
14700 ! need button to show totals
14710     let fncmdset(2)
14720     let fnacs(sn$,0,mat resp$,ck)
14730     if ck<>5 then 
14740       for j=1 to 3
14750         if resp$(j)='True' then let ti1=j : let ckoption=j
14760       next j
14770       let prd=val(resp$(4)) ! date of checks
14780       let prdmmddyy=val(resp$(4)(5:6))*10000+val(resp$(4)(7:8))*100+val(resp$(4)(3:4)) ! convert date back to mmddyy format
14790       let ckn=val(resp$(5)) ! beginning ck number
14800       let bankcode=val(resp$(6)(1:3))
14810       let scc$(1)="SCS"
14820       let scc$(2)="CSS"
14830       let scc$(3)="SSC"
14840       let scc$(4)="SCC"
14850       for j=1 to 4
14860         if trim$(resp$(7))=trim$(item2$(j)) then let scc$=scc$(j)
14870       next j
14880     end if  ! ck<>5
14890     let fn_scr_main_questions=ck
14900   fnend 
14910   def fn_scr_check_entry
14920 ! Let PAS=0
14930     let fntos(sn$="ckprt-3")
14940     let respc=0
14950     let fnfra(1,1,6,87,"Check"," ")
14960     let fnlbl(1,1,cnam$,40,2,0,1)
14970     let fnlbl(2,1,bn$,40,2,0,1)
14980     let fnlbl(3,55,"Amount:",10,1,0,1)
14990     let fntxt(3,67,12,0,1,"10",0,"",1)
15000     let resp$(respc+=1)=tr$(3)
15010     let fnlbl(5,1,"Payee:",8,1,0,1)
15020     let fncombof("Paymstr",5,10,30,"Q:\CLmstr\paymstr.h"&str$(cno),1,8,9,30,"Q:\CLmstr\Payidx1.h"&str$(cno),0,pas, "Enter the payee number or simply enter the payee name if no vendor record exits",1)
15030     let resp$(respc+=1)=holdpayee$
15040     let fnfra(9,1,12,96,"Breakdown Information"," ")
15050     let fnlbl(1,1,"General Ledger",30,0,0,2)
15052     let fnlbl(1,41,"Amount             Description",12,0,0,2)
15054     let fnlbl(1,56,"Description",30,0,0,2)
15060     for j=1 to 10
15070       let fnqgl(j+1,1,2,2)
15080       let resp$(respc+=1)=fnrgl$(resp$(respc))
15082 ! 
15090       let fntxt(j+1,41,12,0,1,"currency",0,"",2)
15100       let resp$(respc+=1)=resp$(respc)
15102 ! 
15110       let fntxt(j+1,56,30,0,0,"",0,"",2)
15120       let resp$(respc+=1)=resp$(respc)
15122 ! 
15130     next j
15140     if screen=2 or screen=3 then 
15150       let fnbutton(12,74,"Back",21,"Previous breakdown screen",1,4,2)
15160     end if 
15170     if screen=0 or screen=1 or screen=2 then 
15180       let fnbutton(12,82,"More",20,"Allows another screen of breakdowns",1,4,2)
15190     end if 
15200     let pas=1 ! don't redo combo boxes
15210     let fnlbl(1,45,"Check Number:",15,1,0,1)
15220     let fntxt(1,62,8,0,1,"30",0,"",1)
15230     let resp$(respc+=1)=str$(ckn)
15240     let fnlbl(3,30,"Check Date:",12,1,0,1)
15250     let fntxt(3,44,10,0,1,"3",0,"",1)
15260     let resp$(respc+=1)=str$(prd)
15270     let fnbutton(5,45,"Add Payee",50,"Click to add a new payee record",0,0,1)
15280     let fncmdkey("Print",1,1,0,"Prnt this check and advance to next check")
15290     let fncmdkey("&Allocate",2,0,0,"Automatically allocates the general ledger breakdown if payee record contains the breakdown information")
15300     let fncmdkey("&Complete",5,0,1,"Return to menu.")
15310 ! need a fncmdkey to change screens for the breakdowns  (screen 1,2 or 3)
15320     let fnacs(sn$,0,mat resp$,ck)
15330     if client$="Brazeal" and ck=5 then let fncloseprn : goto SCE_XIT
15340     if ck=5 then let screen=0 : goto SCE_XIT
15350 ! 
15360     for j=3 to 30, step 3
15370       let resp$(j)=fnagl$(resp$(j))
15380     next j
15390 ! 
15400     if ck=50 then let fn_payee_add : goto CKOPTION1_CHECK_ENTRY_2
15410     let tr$(3)=resp$(1) ! amount
15420     let vn$=tr$(4)=lpad$(rtrm$(resp$(2)(1:8)),8) ! payee number
15430     read #h_paymstr1,using "Form pos 1,c 8",key=vn$,release: vn$ nokey SCE_L4640
15440     let tr$(5)=resp$(2)(9:30) ! payee name
15450     goto SCE_L4650
15460 SCE_L4640: ! 
15470     let tr$(5)=resp$(2)(1:30)
15480     let vn$=tr$(4)="": let b$(1)=tr$(5) ! payee name without vendor record
15490 SCE_L4650: ! 
15500     let ckn=val(resp$(33)) ! ck number
15510     let holdpayee$=resp$(2)
15520     let prd=val(resp$(34)): ! date
15530     let prdmmddyy=val(resp$(34)(5:6))*10000+val(resp$(34)(7:8))*100+val(resp$(34)(3:4)) ! convert date back to mmddyy format
15540     let tr$(2)=cnvrt$("pic(######)",prdmmddyy)
15550 ! STORE_RESPONSES: ! hold all 94 possible responses in holdresp$
15560     let x=0
15570     if screen=0 then let screen=1
15580     for j=1 to 34
15590       if j=1 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! amount
15600       if j=2 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! vendor
15610       if j=33 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! checknumber
15620       if j=34 then let holdresp$(j)=resp$(j): goto SCE_L4820 ! date
15630       if screen=1 then let x=j ! (3-32)
15640       if screen=2 then let x=j+32 ! (35-64)
15650       if screen=3 then let x=j+62 ! (65-94)
15660       if int(j+2/3)=(j+2/3) then 
15670         let holdresp$(x)=resp$(j)
15680       else 
15690         let holdresp$(x)=resp$(j) ! hold all general ledger breakdowns
15700       end if 
15710 SCE_L4820: ! 
15720     next j
15730 SCE_XIT: ! 
15740     let fn_scr_check_entry=ck
15750   fnend 
15760   def fn_read_standard_breakdowns ! pull standard gl breakdowns from payee file
15770     read #h_paymstr1,using "Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,X 6,C 12,C 30,C 50,C 12,C 20",key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey RSB_XIT
15780     mat holdresp$=("")
15790     restore #payeegl,key>=vn$: nokey RSB_EO_READSTGL
15800     let totalalloc=0
15810     for j=3 to 92 step 3
15820       if j=33 or j=34 then goto RSB_L5310 ! skip ck num and date  (resp$(33)&34)
15830 RSB_L5240: ! 
15840       read #payeegl,using "Form Pos 1,C 8,c 12,n 6.2,c 30",release: payeekey$,payeegl$,percent,gldesc$ eof RSB_EO_READSTGL
15850       if vn$<>payeekey$ then goto RSB_EO_READSTGL
15860       if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto RSB_L5310
15870       read #glmstr18,using 'Form POS 13,C 30',key=payeegl$,release: de$ nokey RSB_L5240
15880       let resp$(j)=payeegl$
15890       let resp$(j+1)=str$(round(val(tr$(3))*percent*.01,2))
15900       let totalalloc+=val(resp$(j+1))
15910       let resp$(j+2)=gldesc$ ! description
15920 RSB_L5310: ! 
15930     next j
15940 RSB_EO_READSTGL: ! 
15950     if val(tr$(3))<>totalalloc then 
15960       let resp$(4)=str$(val(resp$(4))+val(tr$(3))-totalalloc)
15970     end if 
15980 RSB_XIT: ! 
15990   fnend 
16000   def fn_write_history
16010     let holdvn$=""
16020     let hck=0
16030     let fn_close(h_paytrans:=4)
16040     open #h_paytrans: "Name=Q:\CLmstr\PayTrans.h"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
16050 WH_LOOP_TOP: ! 
16060     read #h_paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,ckpay,dp,gde eof WH_XIT
16070     if gde=1 then let gde=0 ! dont allow posting code of 1 from unpaid file
16080     if upa=0 then goto WH_L3910
16090     if dp=0 and ckpay=0 then goto WH_LOOP_TOP
16100     if bc=0 then let bc=bankcode ! IF CODED FOR PAYMENT WITH NO BANK CODE, PAY FROM THE CURRENT BANK ACCOUNT
16110     if bc<>bankcode then goto WH_LOOP_TOP
16120     let iv$=rpad$(ltrm$(iv$),12)
16130     read #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8',key=vn$&iv$,release: vn$ nokey WH_L3650
16140     goto WH_L3660
16150 WH_L3650: ! 
16160     write #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8': vn$,iv$,dp,ckpay
16170 WH_L3660: ! 
16180     if vn$=holdvn$ and hck=ckpay then goto WH_L3770
16190     mat tr=(0)
16200     let totalupa=0
16210     let vn$=lpad$(rtrm$(vn$),8)
16220     read #h_paymstr1,using 'form pos 9,c 30',key=vn$,release: b$(1) nokey WH_L3740 ! PAYEE FILE
16230     if ltrm$(vn$)(1:2)="T-" then 
16240       delete #h_paymstr1,key=vn$: nokey ignore ! L10022
16250 L10022: ! 
16260     end if  ! ltrm$(vn$)(1:2)="T-"
16270 WH_L3740: ! 
16280     if holdvn$<>vn$ or (hck<>ck and hck>0) then 
16290       write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bc,1,ckpay,dp,upa,vn$,b$(1),0,0,1
16300     end if  ! holdvn$<>vn$ or (hck<>ck and hck>0)
16310     let holdvn$=vn$
16320     let hck=ck
16330 WH_L3770: ! 
16340     read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WH_L3800
16350     let bal=bal-upa
16360     rewrite #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckpay nokey ignore ! WH_L3800
16370 WH_L3800: ! form pos 1,n 2,n 1,g 8,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
16380     restore #h_unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_EO_UNPDALOC
16390 WH_L3820: ! 
16400     read #h_unpdaloc,using 'Form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$ eof WH_EO_UNPDALOC
16410     if trim$(allockey$(1:8))<>trim$(vn$) or trim$(allockey$(9:20))<>trim$(iv$) then goto WH_EO_UNPDALOC ! if ALLOCKEY$<>VN$&IV$ Then Goto 3690
16420     if sum(agl)=0 and aamt=0 then goto WH_L3820 ! don't allow zero allocations to write
16430     write #tralloc,using 'Form POS 1,N 2,N 1,G 8,G 3,G 6,G 3,PD 5.2,C 30,G 6,x 3,C 12,N 1': bc,1,ckpay,mat agl,aamt,ltrm$(rtrm$(iv$))&" "&ade$(1:17),up$(1),up$(3),gde
16440 ! Let HOLDVN$=VN$
16450     let hck=ckpay
16460     let totalupa+=aamt
16470     goto WH_L3820
16480 WH_EO_UNPDALOC: ! 
16490     rewrite #trmstr1,using 'Form POS 18,pd 10.2',key=lpad$(rtrm$(str$(bc)),2)&"1"&lpad$(rtrm$(str$(ckpay)),8): totalupa
16500 WH_L3910: ! 
16510     delete #h_paytrans: 
16520     do 
16530       delete #h_unpdaloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_L3940
16540     loop 
16550 WH_L3940: ! 
16560     goto WH_LOOP_TOP
16570 WH_XIT: ! 
16580   fnend 
16590   def fn_englishdollar
16600     if ~setup_englishdollar then 
16610       let setup_englishdollar=1
16620       data One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Eleven,Twelve
16630       data Thirteen,Fourteen,Fifteen,Sixteen,Seventeen,Eighteen,Nineteen
16640       data Twenty,Thirty,Forty,Fifty,Sixty,Seventy,Eighty,Ninety
16650       read mat wording$
16660     end if  ! ~setup_englishdollar
16670     let dol=dolamt ! ENGLISH DOLLAR ROUTINE
16680     let n=64
16690     if dol<1000000 and dol>=0 then goto L2760
16700     let eng$="Value too big for editing or was less than zero"
16710     goto ENGLISHDOLLAR_XIT
16720 ! ______________________________________________________________________
16730 L2760: ! 
16740     let eng$="***"
16750     let amount(1)=int(dol*100+.500000001)
16760     for a0=2 to 10
16770       let amount(a0)=int(amount(a0-1)/10+.000000001)
16780     next a0
16790     for a0=1 to 10
16800       let amount(a0)=amount(a0)-amount(a0+1)*10
16810     next a0
16820     if amount(11)+amount(10)+amount(9)=0 then goto L2880
16830     let a0=9
16840     gosub ENGLISHDOLLAR_L3190
16850     let eng$=rtrm$(eng$)&" Million"
16860 L2880: if amount(8)+amount(7)+amount(6)=0 then goto L2920
16870     let a0=6
16880     gosub ENGLISHDOLLAR_L3190
16890     let eng$=rtrm$(eng$)&" Thousand"
16900 L2920: ! 
16910     if amount(5)+amount(4)+amount(3)=0 then goto L2950
16920     let a0=3
16930     gosub ENGLISHDOLLAR_L3190
16940 L2950: ! 
16950     if dol>=1 then goto L2970
16960     let eng$=rtrm$(eng$)&" Zero"
16970 L2970: ! 
16980     let eng$=rtrm$(eng$)&" Dollar"
16990     if dol<2 and dol>=1 then goto L3010
17000     let eng$=rtrm$(eng$)&"s"
17010     if len(rtrm$(eng$))>64 then goto L3010
17020 L3010: ! 
17030     let eng$=rtrm$(eng$)&" and"
17040     if amount(2)+amount(1)=0 then goto L3080
17050     let amount(3)=0
17060     let a0=1
17070     gosub ENGLISHDOLLAR_L3190
17080     goto L3090
17090 ! ______________________________________________________________________
17100 L3080: ! 
17110     let eng$=rtrm$(eng$)&" Zero"
17120 L3090: ! 
17130     let eng$=rtrm$(eng$)&" Cent"
17140     if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3120
17150     let eng$=rtrm$(eng$)&"s"
17160 L3120: ! 
17170     if len(rtrm$(eng$))<64 then goto L3170
17180     for j=1 to 9
17190       let n=65-j
17200       if eng$(n:n)=" " then goto L3170
17210     next j
17220 L3170: ! 
17230     goto ENGLISHDOLLAR_XIT
17240 ! ______________________________________________________________________
17250 ENGLISHDOLLAR_L3190: ! 
17260     if amount(a0+2)=0 then goto L3210
17270     let eng$=rtrm$(eng$)&" "&wording$(amount(a0+2))
17280     let eng$=rtrm$(eng$)&" Hundred"
17290 L3210: if amount(a0+1)=0 and amount(a0)=0 then goto ED_L3190_XIT
17300     if amount(a0+1)<2 then goto L3260
17310     let eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)+18)
17320     if amount(a0)=0 then goto ED_L3190_XIT
17330     let amount(a0+1)=0
17340 L3260: let eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)*10+amount(a0))
17350 ED_L3190_XIT: ! 
17360     return  ! ENGLISHDOLLAR_L3190
17370 ENGLISHDOLLAR_XIT: ! 
17380   fnend 
17390   def fn_msg_allocations_off
17400     mat ml$(3)
17410     let ml$(1)="The net check ("&tr$(3)&") must agree with the total"
17420     let ml$(2)="allocations ("&str$(tac)&").  Correct the allocation"
17430     let ml$(3)="amounts or the net check to proceed."
17440     let fnmsgbox(mat ml$,resp$,cap$,16)
17450   fnend 
17460   def fn_checkdiscount ! check for any discounts
17470     if disamt=0 then goto DISCOUNTRETURN
17480     if ddate<prd then goto DISCOUNTRETURN ! already passed discount date
17490     let upa=upa-disamt
17500     rewrite #h_paytrans,using "form pos 63,n 10.2,pos 97,n 10.2",rec=lr4: upa,0 ! subtract discount, rewrite new unpaid invoice amount and zero discount amt
17510     restore #h_unpdaloc,key>=vn$&iv$: nokey DISCOUNTRETURN ! get the fund # from 1st g/l in the allocation file.
17520     read #h_unpdaloc,using 'Form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$
17530     if trim$(allockey$(1:8))=trim$(vn$) and trim$(allockey$(9:20))=trim$(iv$) then let fundkey$=cnvrt$("pic(ZZZ)",agl(1)) else let fundkey$="   "
17540     let apgl$=discountgl$=""
17550     read #glcontrol, using "Form pos 52,c 12,pos 64,c 12",key=fundkey$: apgl$,discountgl$ nokey MSGBOX6
17560 L6410: ! 
17570     write #h_unpdaloc,using 'Form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,discountgl$,-disamt,"Discount=$"&str$(disamt)
17580 ! create an entry on the unpaid allocation file to record the discount
17590     if method$="A" and pcd>0 then write #h_unpdaloc,using 'Form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,apgl$,disamt,"Discount=$"&str$(disamt) ! create an entry in the unpaid allocation file to record the reduction in accounts payable if accrued and posted
17600     goto DISCOUNTRETURN
17610 MSGBOX6: ! 
17620     mat ml$(5)
17630     let ml$(1)="Invoice # "&trim$(iv$)&" on payee # "&trim$(vn$)&" quallifies for a discount of "&trim$(cnvrt$("pic($$$$$$.##)",disamt))&","
17640     let ml$(2)="but you have not entered the discount G/L # in the G/L control file."
17650     let ml$(3)="The discount will be taken, but the entry in check history will not"
17660     let ml$(4)="contain a G/L number.  Fix the GL # in the transaction file and place the "
17670     let ml$(5)="discount G/L #s in the G/L control file."
17680     let fnmsgbox(mat ml$,resp$,cap$,16)
17690     goto L6410
17700 DISCOUNTRETURN: ! 
17710   fnend 
17720   def fn_write_ck_hist_1 ! WRITE TRANSACTION FOR SINGLE CHECK ENTRY
17730     mat tr=(0)
17740     let tr$(1)=lpad$(str$(ckn),8)
17750     let tr$(4)=lpad$(rtrm$(tr$(4)),8)
17760 ! let k$=lpad$(str$(bankcode),2)&"1"&tr$(1)
17770     let tr$(1)=lpad$(str$(ckn),8)
17780     let tr3=val(tr$(3))
17790     write #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,tr$(1),prdmmddyy,tr3,tr$(4),tr$(5),0,clr,1
17800     read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WCH1_AFTER_WRITE
17810     let bn$=rtrm$(bn$)
17820     let bal=bal-val(tr$(3)) conv ignore
17830     rewrite #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckn nokey WCH1_AFTER_WRITE
17840 WCH1_AFTER_WRITE: ! LET K$=LPAD$(RTRM$(STR$(BANKCODE)),2)&LPAD$(STR$(1),1)&LPAD$(TR$(1),8)
17850     for j=1 to 30
17860       if val(in3$(j*5-1))<>0 then 
17870         let gl$=""
17880         let gl$=cnvrt$("N 3",val(in3$(j*5-4)))&cnvrt$("N 6",val(in3$(j*5-3)))&cnvrt$("N 3",val(in3$(j*5-2)))
17890         let alloc=val(in3$(j*5-1))
17900         let de$=in3$(j*5) ! de$=rtrm$(tr$(5)(1:17))&"-"&in3$(j*5)(1:12)
17910         let tr$(1)=lpad$(str$(ckn),8)
17920         write #tralloc,using 'Form POS 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': bankcode,1,tr$(1),gl$,alloc,de$,0,"",0
17930       end if 
17940     next j
17950     let tr$(2)
17960     mat tr$=("")
17970     mat in3$=("")
17980     if ltrm$(vn$)(1:2)="T-" then delete #h_paymstr1,key=vn$: nokey ignore ! L10782
17990 L10782: ! 
18000   fnend 
18010   def fn_payee_add
18020     let vn$=tr$(4)
18030     mat pr$=("")
18040     mat desc$=("")
18050     mat gl$=("")
18060     let contact$=email$=fax$=myact$=ss$=ph$=""
18070     let fnaddpayee
18080     let pas=0
18090   fnend 
18100   def fn_cknum ! CHECK FOR DUPLICATE CHECK NUMBERS
18110 CKNUM_TOP: ! CHECK FOR DUPLICATE CHECK NUMBERS
18120     let dk$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(ckn),8)
18130     read #trmstr1,using 'Form POS 4,C 8,G 6,pd 10.2,C 8,C 35',key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey CKNUM_XIT
18140     let dtr$(3)=str$(dtr3)
18150 SCR_CKPRT6: ! 
18160     let fntos(sn$="ckprt-6")
18170     let respc=0
18180     let fnlbl(1,1,"Check number "&str$(ckn)&" has been previously used.",45,1)
18190     let fnlbl(2,10," Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",val(dtr$(2))),45,0)
18200     let fnlbl(3,10," Amount: "&dtr$(3),45,0)
18210     let fnlbl(4,10," To: "&dtr$(5),45,0)
18220     let fnchk(6,48,"Delete the previous entry:",1)
18230     let resp$(respc+=1)="False"
18240     let fnlbl(8,1,"New check number (if applicable):",45,1)
18250     let fntxt(8,48,8,0,1,"30",0,"You will never enter the new check number if you are deleting the old check.")
18260     let resp$(respc+=1)=""
18270 ! ______________________________________________________________________
18280     let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
18290     let ckn2=val(resp$(2))
18300     if resp$(1)(1:1)="T" then goto CKNUM_DEL_PRV ! delete previous check
18310     if ckn2<=0 then 
18320       mat ml$(2)
18330       let ml$(1)="You must supply the new check number any time"
18340       let ml$(2)="you choose not to delete the old check."
18350       let fnmsgbox(mat ml$,resp$,cap$,16)
18360       goto SCR_CKPRT6
18370     end if 
18380     let ckn=ckn2
18390     let tr$(1)=lpad$(str$(ckn2),8)
18400     goto CKNUM_TOP ! ***********************
18410 CKNUM_DEL_PRV: ! 
18420     let bal=bal+val(dtr$(3))
18430     delete #trmstr1,key=dk$: 
18440     rewrite #bankmstr,using 'Form POS 45,PD 6.2',key=lpad$(str$(bankcode),2): bal nokey ignore ! L10872
18450 L10872: ! 
18460     restore #tralloc,key>=dk$: nokey CKNUM_XIT
18470     do 
18480       read #tralloc,using 'Form POS 1,c 11': trkey$ eof CKNUM_XIT
18490       if trkey$<>dk$ then goto CKNUM_XIT
18500       delete #tralloc,key=dk$: 
18510     loop 
18520 CKNUM_XIT: ! 
18530   fnend 
18540   def fn_index
18550 L4050: ! 
18560     open #31: "Name=Q:\CLmstr\PayTrans.h"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",NoShr",internal,outin,keyed ioerr L4070
18570     close #31: ! 
18580     goto L4080
18590 L4070: ! 
18600     mat ml$(2)
18610     let ml$(1)="You must get everyone out of the Unpaid Invoice File"
18620     let ml$(2)="before you can continue!  Press OK when ready."
18630     let fnmsgbox(mat ml$,resp$,cap$,16)
18640     goto L4050
18650 L4080: ! 
18660     let fn_close(h_unpdaloc)
18670     let fn_close(ivpaid)
18680     execute "Copy Q:\CLmstr\PayTrans.h"&str$(cno)&" "&env$('temp')&"\X -D"
18690     execute "Free Q:\CLmstr\PayTrans.h"&str$(cno)
18700     execute "ReName "&env$('temp')&"\X Q:\CLmstr\PayTrans.h"&str$(cno)
18710     execute "Copy Q:\CLmstr\UnPdAloc.h"&str$(cno)&" "&env$('temp')&"\X -D"
18720     execute "Free Q:\CLmstr\UnPdAloc.h"&str$(cno)
18730     execute "ReName "&env$('temp')&"\X Q:\CLmstr\UnPdAloc.h"&str$(cno)
18740     execute "Index Q:\CLmstr\PayTrans.h"&str$(cno)&",Q:\CLmstr\UNPdIdx1.h"&str$(cno)&",1,20,Replace,DupKeys"
18750     execute "Index Q:\CLmstr\unpdaloc.H"&str$(cno)&",Q:\CLmstr\Uaidx2.H"&str$(cno)&",1,20,Replace,DupKeys -n"
18760     execute "Index Q:\CLmstr\unpdaloc.H"&str$(cno)&",Q:\CLmstr\Uaidx1.H"&str$(cno)&",9,12,Replace,DupKeys -n"
18770     execute "Index Q:\CLmstr\IvPaid.h"&str$(cno)&",Q:\CLmstr\IVIndex.h"&str$(cno)&",1,20,ReOrg,DupKeys"
18780   fnend 
18790 ! 
18800   def fn_portion_stub
18810     if client$="Brazeal" then 
18820       let fn_portion_stub_brazeal
18830     else if client$="Eldorado" then 
18840       let fn_portion_stub_eldorado
18850     else if client$="Billings" then 
18860       let fn_portion_stub_billings
18870     else if client$="Divernon" then 
18880       let fn_portion_stub_divernon
18890     else if client$="Miller Hardware" then 
18900       let fn_portion_stub_miller
18910     else 
18920       let fn_portion_stub_generic
18930     end if 
18940   fnend 
18950   def fn_portion_stub_generic
18960     if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey L10970
18970 L10970: ! 
18980     print #255: ""
18990     print #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19000     print #255: ""
19010     mat b$=(" ") : let b$(1)=tr$(5)(1:30)
19020     if h_vf1=23 then let vp1=173 else let vp1=147
19030     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore ! L10982
19040 ! L10982: !
19050     print #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19060     print #255: "_______________________________________ _______________________________________"
19070     for j=1 to 15
19080       print #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19090     next j
19100   fnend 
19110   def fn_portion_stub_billings
19140     print #255: ""
19150     if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19170     print #255: ""
19180     print #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19200     mat b$=(" ") : let b$(1)=tr$(5)(1:30)
19210     if h_vf1=23 then let vp1=173 else let vp1=147
19220     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19230     print #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19240     print #255: "_______________________________________ _______________________________________"
19250     for j=1 to 15
19260       print #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19270     next j
19272     print #255: ""
19274     print #255: ""
19276     print #255: ""
19280   fnend 
19290   def fn_portion_stub_divernon
19300     print #255: ""
19310     if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19320     print #255: ""
19330     print #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19340     print #255: ""
19350     mat b$=(" ") : let b$(1)=tr$(5)(1:30)
19360     if h_vf1=23 then let vp1=173 else let vp1=147
19370     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19380     print #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19390     print #255: "_______________________________________ _______________________________________"
19400     for j=1 to 15
19410       print #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19420     next j
19430   fnend 
19440   def fn_portion_stub_eldorado
19450     if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19460     mat b$=(" ") : let b$(1)=tr$(5)(1:30)
19470     if h_vf1=23 then let vp1=173 else let vp1=147
19480     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19490     if ckn1<>psb_ckn1_prior then ! it's the first stub on a check
19500       let psb_ckn1_prior=ckn1
19510       print #255,using 'Form POS 74,N 6': ckn1 : print ckn1
19520       print #255: ""
19530       print #255: ""
19540 F_PSE_ITEM: form pos 1,c 9,c 30,pic(zzzz,zzz.zzcr)
19550       for j=1 to 15
19560         print #255,using F_PSE_ITEM: iv$(j,1)(1:9),de$(j,1),amt(j,1)
19570       next j
19580       for j=1 to 3
19590         print #255,using F_PSE_ITEM: iv$(j,2)(1:9),de$(j,2),amt(j,2)
19600       next j
19610     else ! it's the second stub on a check
19620       print #255: ""
19630       print #255: "" ! print #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19640       print #255: ""
19650       print #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
19660       print #255: "_______________________________________ _______________________________________"
19670       for j=1 to 15
19680         print #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
19690       next j
19700     end if  ! ckn1<>psb_ckn1_prior   /   else 
19710   fnend 
19720   def fn_portion_stub_brazeal
19730     if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
19740     print #255: "" : print #255: : print #255: : print #255: : print #255: 
19750     print #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19760     print #255: "" : print #255: 
19770     mat b$=(" ") : let b$(1)=tr$(5)(1:30)
19780     if h_vf1=23 then let vp1=173 else let vp1=147
19790     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
19800     let xx=0
19810     for j=1 to 5
19820       do 
19830         let xx+=1
19840         print #255,using 'Form POS 1,C 12,x 1,pic(zz/zz/zz),x 1,c 22,pIC(ZZZ,ZZZ.ZZCR),x 1,2*pIC(ZZZZ,ZZZ.ZZCR)': iv$(j,xx)(1:12),ivdate(j,xx),de$(j,xx)(1:22),amt(j,xx),disamt(j,xx),amt(j,xx)-disamt(j,xx)
19850       loop while xx<2
19860       if xx=2 then let xx=0
19870     next j
19880   fnend 
19890   def fn_portion_stub_miller
19900     if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey L19910
19910 L19910: ! 
19920     print #255: ""
19930     print #255,using 'Form SKIP 5,POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
19940     print #255: ""
19950     mat b$=(" ") : let b$(1)=tr$(5)(1:30)
19960     if h_vf1=23 then let vp1=173 else let vp1=147
19970     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore ! L10982
19980 ! L10982: !
19990     print #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
20000     print #255: "_______________________________________ _______________________________________"
20010     for j=1 to 15
20020       print #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
20030     next j
20040   fnend 
20050 ! 
20060   def fn_portion_check
20062     if client$="Ash Grove" then 
20064       let fn_portion_check_ashgrove(amt)
20066     else if client$="Billings" then 
20068       let fn_portion_check_billings(amt)
20070     else if client$="Brazeal" then 
20080       let fn_portion_check_brazeal(amt)
20090     else if client$="Divernon" then 
20100       let fn_portion_check_divernon(amt)
20110     else if client$="Eldorado" then 
20120       let fn_portion_check_eldorado(amt)
20130     else if client$="Kimberling" then 
20140       let fn_portion_check_kimber(amt)
20150     else if client$="ACS" and bankcode=2 then 
20160       let fn_portion_check_acs(amt)
20170 !   else if client$="Washington Parrish" and cno=17 then 
20180 !     let fn_portion_check_washparrish(amt)
20190 !   else if client$="Washington Parrish" and (cno=16 or cno=19 or cno=66666) then 
20200 !     let fn_portion_check_washparrish2(amt)
20210     else if client$="Cerro Gordo" then 
20220       let fn_portion_check_cerrogordo(amt)
20230     else if client$="Miller Hardware" then 
20240       let fn_portion_check_miller(amt)
20250     else 
20260       let fn_portion_check_generic(amt)
20270     end if 
20280   fnend 
20290   def fn_portion_check_generic(dolamt)
20300     mat b$=("")
20310     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
20320     let fn_englishdollar
20330     x=1 ! if client$="Washington Parrish" then let x=2 else let x=1
20340     if client$="ACS" or client$="Thomasboro" or client$="Hope Welty" or client$="Philo" or client$="Billings" or client$="Divernon" then goto L1730 ! don't skip
20350     if client$="Battlefield" then let x=1 ! 3
20360     for j=1 to x
20370       print #255: ""
20380     next j
20390 L1730: if dolamt=0 then let eng$='        *** V O I D ***'
20400     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
20410     if rtf$="Y" then let skipline=9 else let skipline=4
20420     if client$="Divernon" then let skipline=8
20430     if client$="Billings" then 
20432       let skipline=2
20434       print #255: 
20436       print #255,using 'form pos 40,c 38,skip 1': "Void After 60 Days"
20438       print #255: 
20440 !     print #255:
20442     end if 
20450     if prenum=2 then print #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
20460     if prenum=2 then let skipline=max(skipline-3,1)
20470 !   if client$="Cerro Gordo" then let skipline=skipline+2
20490     if client$<>'Battlefield' then 
20500       print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
20510     else 
20520       print #255,using 'Form SKIP SKIPLINE,pos 1,c 1': ''
20530     end if 
20540     print #255: ""
20550     if rtf$="Y" then let a=65 else let a=55
20560     let normal4=4
20570     if client$="Bethany" then let a=54
20580     if client$='Battlefield' then let a=60 : let normal4=6
20590     if client$="Unity" then let a=55
20600     if client$="Thomasboro" or client$="Hope Welty" or client$="Philo" or client$="Monticello" or client$="Edinburg" or client$="Divernon" then let a=55
20601     if client$="Billings" then let a=59
20610     print #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X normal4,C 18': prdmmddyy,ca$
20620     if client$="Billings" then print #255: ! 51010
20630     print #255: ""
20640     if client$<>'Battlefield' then print #255: ""
20650     if client$='Battlefield' then 
20660       print #255,using 'Form POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
20670     end if 
20680     if client$='Battlefield' then print #255: ""
20690 ! if client$="Thomasboro" then print #255: : print #255:
20700     if trim$(b$(2))="" then 
20710       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
20720     else if trim$(b$(3))="" then 
20730       let b$(3)=b$(4) : let b$(4)=""
20740     end if 
20750     for j=1 to 4
20760       print #255,using "Form Pos 8,C 30": b$(j)
20770     next j
20780     if rtf$="Y" then let skipline=6 else let skipline=5
20790     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
20800 !   if client$="Washington Parrish" then let skipline=skipline-1
20810     if client$="ACS" or client$="Hope Welty" or client$="Philo" or client$="Thomasboro" then let skipline=skipline+2
20820     if client$="Divernon" or client$='Battlefield' then let skipline=skipline+1
20830     if client$="Billings" then let skipline=skipline+1 ! 51010
20840     if client$="PiattCO" then let skipline=skipline+4
20850     for j=1 to skipline
20860       print #255: ""
20870     next j
20880   fnend 
22000   def fn_portion_check_ashgrove(dolamt)
22020     mat b$=("")
22040     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
22060     let fn_englishdollar
22080     let x=3 ! 1  add three lines after top stub
22100     for j=1 to x
22120       print #255: ""
22140     next j
22160     if dolamt=0 then let eng$='        *** V O I D ***'
22180     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
22200     if rtf$="Y" then let skipline=9 else let skipline=4
22220     if prenum=2 then print #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
22240     if prenum=2 then let skipline=max(skipline-3,1)
22260     print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
22280 !   print #255: ""
22300     if rtf$="Y" then let a=65 else let a=55
22320     let normal4=4
22340     print #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X normal4,C 18': prdmmddyy,ca$
22360 !   print #255: ""
22380     print #255: ""
22400     if trim$(b$(2))="" then 
22420       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
22440     else if trim$(b$(3))="" then 
22460       let b$(3)=b$(4) : let b$(4)=""
22480     end if 
22500     for j=1 to 4
22520       print #255,using "Form Pos 8,C 30": b$(j)
22540     next j
22560     if rtf$="Y" then let skipline=6 else let skipline=5
22580     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
22600     for j=1 to skipline+3
22620       print #255: ""
22640     next j
22660   fnend 
22680   def fn_portion_check_eldorado(dolamt)
22700     mat b$=("")
22720     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey PCE_L1680
22740     if trim$(b$(2))="" then 
22760       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
22780     else if trim$(b$(3))="" then 
22800       let b$(3)=b$(4) : let b$(4)=""
22820     end if 
22840 PCE_L1680: ! 
22860     let fn_englishdollar
22880     if dolamt=0 then let eng$='        *** V O I D ***'
22900     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
22920     print #255: ''
22940     print #255: ''
22960     print #255: ''
22980     if prenum=2 then 
23000       print #255,using "form pos 74,n 8": ckn1
23020     else 
23040       print #255: ''
23060     end if 
23080     print #255: ''
23100     print #255: ''
23120     print #255: ''
23140     print #255,using 'form pos 68,pic(zz/zz/zz)': prdmmddyy
23160     print #255: ''
23180     print #255: ''
23200     print #255: ''
23220     print #255,using 'Form Pos 9,C 80': eng$(1:n)
23240     print #255,using 'Form Pos 9,C 70': eng$(n+1:128)
23260     print #255,using 'Form POS 65,X 8,X 4,C 18': ca$
23280     print #255: ''
23300     print #255: ''
23320     for j=1 to 4
23340       print #255,using "Form Pos 8,C 30": b$(j)
23360     next j
23380     print #255: ''
23400     print #255: ''
23420     print #255: ''
23440     print #255: ''
23460     print #255: ''
23480     print #255: ''
23500   fnend 
23520   def fn_portion_check_kimber(dolamt)
23540     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6480
23560     goto L6490
23580 L6480: mat b$=("")
23600 L6490: let fn_englishdollar
23620     let x=1
23640     for j=1 to x
23660       print #255: ""
23680     next j
23700     if dolamt=0 then let eng$='        *** V O I D ***'
23720     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
23740     let skipline=9
23760     if prenum=2 then print #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
23780     if prenum=2 then let skipline=max(skipline-3,1)
23800     print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
23820     print #255: ""
23840     print #255,using 'Form POS 60,PIC(ZZ/ZZ/ZZ),X 7,C 18': prdmmddyy,ca$
23860     print #255: ""
23880     print #255: ""
23900     if trim$(b$(2))="" then 
23920       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
23940     else if trim$(b$(3))="" then 
23960       let b$(3)=b$(4) : let b$(4)=""
23980     end if 
24000     for j=1 to 4
24020       print #255,using "Form Pos 8,C 30": b$(j)
24040     next j
24060     let skipline=8
24080     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
24100     for j=1 to skipline
24120       print #255: ""
24140     next j
24160   fnend 
24180   def fn_portion_check_divernon(dolamt)
24200     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6690
24220     goto L6700
24240 L6690: mat b$=("")
24260 L6700: let fn_englishdollar
24280     let x=1
24300     for j=1 to x
24320       print #255: ""
24340     next j
24360     if dolamt=0 then let eng$='        *** V O I D ***'
24380     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
24400     for j=1 to 9
24420       print #255: " "
24440     next j
24460     let a=62
24480     print #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
24500     let skipline=2
24520     print #255,using 'Form SKIP SKIPLINE,POS 4,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
24540     if trim$(b$(2))="" then 
24560       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
24580     else if trim$(b$(3))="" then 
24600       let b$(3)=b$(4) : let b$(4)=""
24620     end if 
24640     for j=1 to 4
24660       print #255,using "Form Pos 8,C 30": b$(j)
24680     next j
24700     let skipline=6
24720     for j=1 to skipline
24740       print #255: ""
24760     next j
24780   fnend 
24800   def fn_portion_check_washparrish(dolamt) !   company #17 only
24820     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6910
24840     goto L6920
24860 L6910: mat b$=("")
24880 L6920: let fn_englishdollar
24900     if dolamt=0 then let eng$='        *** V O I D ***'
24920     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
24940     for j=1 to 10
24960       print #255: " "
24980     next j
25000     print #255,using 'Form POS 4,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
25020     print #255,using 'Form skip 1,POS 62,PIC(ZZ/ZZ/ZZ),X 4,C 18,skip 2': prdmmddyy,ca$
25040     if trim$(b$(2))="" then 
25060       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
25080     else if trim$(b$(3))="" then 
25100       let b$(3)=b$(4) : let b$(4)=""
25120     end if 
25140     for j=1 to 4
25160       print #255,using "Form Pos 8,C 30": b$(j)
25180     next j
25200     for j=1 to 6
25220       print #255: ""
25240     next j
25260   fnend 
25280   def fn_portion_check_washparrish2(dolamt) ! companies 45,2,16,19,66666
25300     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L7080
25320     goto L7090
25340 L7080: mat b$=("")
25360 L7090: let fn_englishdollar
25380     for j=1 to 2
25400       print #255: ""
25420     next j
25440     if dolamt=0 then let eng$='        *** V O I D ***'
25460     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
25480     if rtf$="Y" then let skipline=9 else let skipline=4
25500     if prenum=2 then print #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
25520     if prenum=2 then let skipline=max(skipline-3,1)
25540     print #255,using 'Form skip 6,pos 75,PIC(ZZ/ZZ/ZZ)': prdmmddyy: let skipline=3
25560     print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,skIP 1,POS 9,C 60,c 14': eng$(1:n), eng$(n+1:118),ca$
25580     print #255: "" : goto L7190
25600     print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
25620     print #255: ""
25640 L7190: if rtf$="Y" then let a=65 else let a=55
25660     print #255: ""
25680     print #255: ""
25700     if trim$(b$(2))="" then 
25720       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
25740     else if trim$(b$(3))="" then 
25760       let b$(3)=b$(4) : let b$(4)=""
25780     end if 
25800     for j=1 to 4
25820       print #255,using "Form Pos 8,C 30": b$(j)
25840     next j
25860     if rtf$="Y" then let skipline=6 else let skipline=5
25880     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
25900     let skipline=skipline-1
25920     for j=1 to skipline
25940       print #255: ""
25960     next j
25980   fnend 
26000   def fn_portion_check_brazeal(dolamt)
26020     let dolamt=amt
26040     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L7320
26060     goto L7330
26080 L7320: mat b$=("")
26100 L7330: let fn_englishdollar
26120     for j=1 to 3
26140       print #255: ""
26160     next j
26180     if dolamt=0 then let eng$='        *** V O I D ***'
26200     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
26220     for j=1 to 8
26240       print #255: " "
26260     next j
26280     let a=62
26300     print #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X 4,pic(zzzzzzz)': prdmmddyy,ckn
26320     let skipline=2
26340     print #255,using 'Form SKIP SKIPLINE,POS 4,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
26360     print #255,using 'form pos 64,c 15': ca$
26380     print #255: : print #255: 
26400     if trim$(b$(2))="" then 
26420       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
26440     else if trim$(b$(3))="" then 
26460       let b$(3)=b$(4) : let b$(4)=""
26480     end if 
26500     for j=1 to 4
26520       print #255,using "Form Pos 8,C 30": b$(j)
26540     next j
26560     let skipline=2
26580     for j=1 to skipline
26600       print #255: ""
26620     next j
26640   fnend 
26660   def fn_portion_check_acs(dolamt)
26680     mat b$=("")
26700     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
26720     if trim$(b$(2))="" then 
26740       let b$(2)=b$(3) : let b$(3)=b$(4) : let b$(4)=""
26760     else if trim$(b$(3))="" then 
26780       let b$(3)=b$(4) : let b$(4)=""
26800     end if 
26820     let fn_englishdollar
26840     if dolamt=0 then let eng$='        *** V O I D ***'
26860     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
26880     print #255: ""
26900     let skipline=9
26920     if prenum=2 then print #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
26940     if prenum=2 then let skipline=max(skipline-3,1)
26960     print #255,using "form skip 1, pos 80,pic(zz/zz/zz)": prdmmddyy
26980     print #255: ""
27000     print #255,using 'Form skip 1,POS 15,C 30,pos 73,c 18': b$(1),ca$
27020     print #255: ""
27040     print #255,using 'Form SKIP 1,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n),eng$(n+1:128)
27060     print #255: ""
27080     for j=1 to 4
27100       print #255,using "Form Pos 8,C 30": b$(j)
27120     next j
27140     let skipline=10
27160     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
27180     for j=1 to skipline
27200       print #255: ""
27220     next j
27240   fnend 
27260   def fn_portion_check_cerrogordo(dolamt)
27280     mat b$=("")
27300     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
27320     let fn_englishdollar
27340     let x=2
27360     for j=1 to x
27380       print #255: ""
27400     next j
27420 L23060: if dolamt=0 then let eng$='        *** V O I D ***'
27440     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
27460     if rtf$="Y" then let skipline=8 else let skipline=3
27480 L23090: form pos 40,c 38,skip 1
27500     if prenum=2 then print #255,using "form skip 3,pos 74,n 8,skip 1": ckn1
27520     if prenum=2 then let skipline=max(skipline-3,1)
27540     let skipline=skipline-2
27560     print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70,SKIP 1': eng$(1:n), eng$(n+1:128)
27580     print #255: ""
27600     if rtf$="Y" then let a=55 else let a=45
27620     let normal4=4
27640     print #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X normal4,C 18': prdmmddyy,ca$
27660     print #255: ""
27680     print #255: ""
27700     print #255: ""
27720     print #255: ""
27740     if trim$(b$(2))="" then 
27760       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
27780     else if trim$(b$(3))="" then 
27800       let b$(3)=b$(4) : let b$(4)=""
27820     end if 
27840     for j=1 to 4
27860       print #255,using "Form Pos 8,C 30": b$(j)
27880     next j
27900     if rtf$="Y" then let skipline=6 else let skipline=5
27920     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
27940     for j=1 to skipline
27960       print #255: ""
27980     next j
28000   fnend 
28020   def fn_portion_check_miller(dolamt)
28040     mat b$=("")
28060     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
28080     let fn_englishdollar
28100     let x=1
28120     for j=1 to x
28140       print #255: ""
28160     next j
28180     if dolamt=0 then let eng$='        *** V O I D ***'
28200     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
28220     let skipline=4
28240     form pos 40,c 38,skip 1
28260     if prenum=2 then print #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
28280     if prenum=2 then let skipline=1
28300     print #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
28320     print #255: "" : print #255: ""
28340     if rtf$="Y" then let a=65 else let a=55
28360     let normal4=10
28380     print #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X normal4,C 18': prdmmddyy,ca$
28400     print #255: ""
28420     print #255: ""
28440     if trim$(b$(2))="" then 
28460       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
28480     else if trim$(b$(3))="" then 
28500       let b$(3)=b$(4) : let b$(4)=""
28520     end if 
28540     for j=1 to 4
28560       print #255,using "Form Pos 8,C 30": b$(j)
28580     next j
28600     let skipline=4
28620     if scc$="SCC" then let skipline=skipline-1 ! don't space as far if stub,check,check
28640     for j=1 to skipline
28660       print #255: ""
28680     next j
28700   fnend 
30000   def fn_portion_check_billings(dolamt)
30020     mat b$=("")
30040     read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
30060     if trim$(b$(2))="" then 
30080       let b$(2)=b$(3): let b$(3)=b$(4) : let b$(4)=""
30100     else if trim$(b$(3))="" then 
30120       let b$(3)=b$(4) : let b$(4)=""
30140     end if 
30160     let fn_englishdollar
30180     if dolamt=0 then let eng$='        *** V O I D ***'
30200     if dolamt<=0 then let ca$="***VOID***" else let ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
30202 ! 
30220     print #255: ""
30240     print #255,using 'form pos 45,c 38': "Void After 60 Days"
30260     print #255: ""
30280     print #255: ""
30300     print #255: ""
30320     if trim$(eng$(n+1:128))='' then 
30340       print #255: ""
30360       print #255,using 'Form POS 9,C 80': eng$(1:n)
30380     else 
30400       print #255,using 'Form POS 9,C 80': eng$(1:n)
30420       print #255,using 'Form POS 9,C 70': eng$(n+1:128)
30440     end if 
30460     print #255,using 'Form POS 59,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
30480     print #255: ""
30500     print #255: ""
30520     print #255: ""
30540     print #255,using "Form Pos 8,C 30": b$(1)
30560     print #255,using "Form Pos 8,C 30": b$(2)
30580     print #255,using "Form Pos 8,C 30": b$(3)
30600     print #255,using "Form Pos 8,C 30": b$(4)
30620     print #255: ""
30640     print #255: ""
30660     print #255: ""
30680     print #255: ""
30700     print #255: ""
30720     print #255: ""
30740     print #255: ""
30760   fnend 
