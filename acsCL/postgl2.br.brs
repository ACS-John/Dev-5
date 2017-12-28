00010 ! Replace S:\acsCL\PostGL2.br
00020 ! GL Distribution Report (fnpostgl2(2)   AND/OR     Post to General Ledger (fnpostgl2(1) - or run directly)

00040   library 'S:\Core\Library': fnpostgl2,fnxit,fnerror,fntop
00050 ! on error goto ERTN
00070   fntop(program$,"Post to General Ledger")
00080   fnpostgl2(1)
00090   fnxit
10000   def library fnpostgl2(glt)
10020     library 'S:\Core\Library': fnopenprn,fncloseprn,fnerror,fnputcno,fndate_mmddyy_to_ccyymmdd,fnprocess,fnchain,fnTos,fnLbl,fnTxt,fncomboa,fnChk,fnCmdSet,fnAcs,fnmsgbox,fnfree
10040     on error goto ERTN
10060 ! ______________________________________________________________________
10080 ! GLT: 1=Post  2=Print Only
10100     glt_post=1
10120     glt_print_only=2
10140 ! 
10160     dim de$*30,tbc(99,2),pde$*30
10180     dim apc(99,3),td$*30,prd(23),cap$*128,glwk$*256,opt_cash_or_accrual$(2)*12,ml$(3)*100
10200 ! 
10220     opt_cash_or_accrual$(1)="Cash"
10240     opt_cash_or_accrual$(2)="Accrual"
10260 ! ______________________________________________________________________
10280     if glt=glt_print_only then 
10300       cap$="GL Distribution Report"
10310       xitable$='YES'
10320     else 
10340       cap$="Post to General Ledger"
1035       xitable$='NO'
10360     end if 
10400 ! 
12000 ! r: determine if cash or accrual by checking for any accounts payable numbers in the general ledger control file
12160     up1$="C"
12180     open #fundmstr=9: "Name="&env$('Q')&"\CLmstr\FundMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\FundIdx1.H"&env$('cno')&",Shr",internal,input,keyed 
12200 READ_FUNDMSTR: ! 
12220     read #fundmstr,using 'Form Pos 52,C 12': gw$ eof EO_FUNDMSTR
12240     accrual=val(gw$) conv L230
12260     if accrual>0 then 
12280       up1$="A"
12300       goto EO_FUNDMSTR
12320     end if 
12340 L230: ! 
12360     goto READ_FUNDMSTR
12380 EO_FUNDMSTR: ! /r
12400     fnTos(sn$="postgl2")
12440     fnLbl(1,60,"",0,1)
12460     fnLbl(1,1,"Starting Date:",44,1)
12480     fnTxt(1,46,10,0,1,"3",0,"Normally this would be the first day of the month.  If you post more often than once a month, it would be the first day of the period you are posting.")
12500     resp$(1)=""
12520     fnLbl(2,1,"Ending Date:",44,1)
12540     fnTxt(2,46,10,0,1,"3",0,"Normally this would be the last day of the month, unless you post more often than once a month!")
12560     resp$(2)=date$('ccyymmdd') ! ""
12580     fnChk(4,47,"Include previously posted transactions:",1)
12600     resp$(3)="False"
12620     fnLbl(5,1,"Basis for Accounting:",44,1)
12680     fncomboa("opt_cash_or_accrual",5,46,mat opt_cash_or_accrual$,"If you record expenses as they are paid, you are on a cash basis.  If you wish to record unpaid invoices (accounts payable) as well as paid expenses, you are on an accrual basis.")
12700     resp$(4)=opt_cash_or_accrual$(1)
12720     fnChk(6,47,"Combine Payroll Entries:",1)
12740     resp$(5)="True"
12760     fnChk(7,47,"Print General Ledger Distribution Listing:",1)
12780     resp$(6)="True"
12800     fnChk(8,47,"Update After the Fact Payroll records:",1)
12820     resp$(7)="False"
12840     fnLbl(10,1,"Post to General Ledger Company Number:",44,1)
12860     fnTxt(10,46,5,0,1,"30",0,"Only change this default answer if wish to post to a different company than the one you are assigned to.")
12880     resp$(8)=env$('cno')
12900     fnCmdSet(2)
12920     fnAcs(sn$,0,mat resp$,ck)
14000     if ck=5 then goto XIT
14020     dt1=val(resp$(1)) ! beginning date
14040     dt2=val(resp$(2)) ! ending date
14060     d1=val(resp$(1)(5:6))*10000+val(resp$(1)(7:8))*100+val(resp$(1)(3:4)) ! beginning date
14080     d2=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! ending date  ! convert dates back to mmddyy
14100     if resp$(3)(1:1)="T" then include_prev_posted$="Y" else include_prev_posted$="N" ! include previously posted entries
14120     if resp$(4)(1:1)="C" then up1$="C" else up1$="A" ! cash or accrual
14140     if resp$(5)(1:1)="T" then prc$="Y" else prc$="N" ! combine payroll entries
14160     if resp$(6)(1:1)="T" then pr1$="Y" else pr1$="N" ! pr distribution listing
14180     if resp$(7)(1:1)="T" then pr2$="Y" else pr2$="N" ! update after fact payroll
14200     gl2=val(resp$(8)) ! GL company to post
16000     if pr2$="Y" then let fnprocess(4)
16020     if glt=glt_print_only then pr1$="Y"
16040     fnputcno(gl2)
16060 !   pr f "13,34,C 12,B,99": "Cancel (Esc)"
16080 !   on fkey 99 goto XIT
16100     fnopenprn
16120     open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
16140     open #tralloc=3: "Name="&env$('Q')&"\CLmstr\TrAlloc.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\tralloc-idx.h"&env$('cno')&",Shr",internal,outIn,keyed 
16160     open #bankmstr=4: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
16180     open #work=5: "Name="&env$('Temp')&"\WORK."&session$&",SIZE=0,RecL=76,Replace",internal,output 
16200     if ~fn_check_breakdowns_add_up then goto XIT ! gosub CHECK_BREAKDOWNS
16220     gosub GLBUCKET_STUFF
16240 ! Gosub GLCHG
18000 READ_TRMSTR: ! r: main loop
18020 L630: read #trmstr,using 'Form POS 1,n 2,n 1,C 8,N 6,PD 10.2,POS 28,C 8,C 30,POS 71,N 1,X 6,N 1': trbank_code,trtcde,ck$,pd,ca1,vn$,de$,pcde,scd eof END1
18040     if scd=4 and fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto L630
18060     if fndate_mmddyy_to_ccyymmdd(pd)<dt1 then goto L630
18080     restore #tralloc,key>=cnvrt$("pic(zz)",trbank_code)&cnvrt$("pic(#)",trtcde)&ck$: nokey L630
18100 READ_TRALLOC: ! 
18120     read #tralloc,using 'Form POS 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,C 6,POS 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd$,gde eof READ_TRMSTR
18130     ivd=val(ivd$) conv ignore
18140     if up1$="C" and gde=2 then gde=1 ! don't allow old accrual codes mess up cash basis
18160     if ivd=0 then ivd=pd ! kJ   10/02/06   skipping receipts w/o invoice numbers
18180     if bank_code<>trbank_code or trtcde<>tcde or ck$<>trck$ then goto L1040 ! thru, allocation doesn/t belong to this transaction
18200     if amt=0 then goto READ_TRALLOC ! SKIP 0
18220     if scd=4 then gosub PRDBLD
18240     if scd=4 or fndate_mmddyy_to_ccyymmdd(ivd)>fndate_mmddyy_to_ccyymmdd(pd) then ivd=pd
18260 ! gde=1  never in ap - entered and paid same month
18280 ! gde=2  posted to ap from unpaid file
18300 ! gde=3  previously posted to ap  - now posted from ck history
18320     if include_prev_posted$="Y" then goto L800
18340     if gde=1 or gde=3 then goto READ_TRALLOC
18360 L800: if scd><4 then goto L820
18380     if fndate_mmddyy_to_ccyymmdd(ivd)>=dt1 and fndate_mmddyy_to_ccyymmdd(ivd)<=dt2 then 
18400       goto L990
18420     else 
18440       goto READ_TRALLOC
18460     end if 
18480 L820: ! 
18500     if ivd>0 then goto L850
18520     if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto READ_TRALLOC
18540     ivd=pd: goto L990 ! Manual Check
18560 L850: ! 
18580     if up1$="C" then ivd=pd
18600     if fndate_mmddyy_to_ccyymmdd(ivd)>dt2 then goto READ_TRALLOC
18620     if up1$="A" and gde=2 and fndate_mmddyy_to_ccyymmdd(ivd)<dt1 then goto L900
18640     if up1$="A" and include_prev_posted$="Y" and gde=3 and fndate_mmddyy_to_ccyymmdd(ivd)<dt1 then goto L900
18660     if up1$="C" or gde<2 then goto L920
18680 L900: ! 
18700     if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto L920
18720     gosub REVERSE_AP
18740     goto L1000
18760 L920: ! 
18780     if include_prev_posted$="Y" and gde=1 and fndate_mmddyy_to_ccyymmdd(pd)<=dt2 then goto L990
18800     if gde=0 and fndate_mmddyy_to_ccyymmdd(pd)<=dt2 then goto L990
18820     if fndate_mmddyy_to_ccyymmdd(ivd)<dt1 or fndate_mmddyy_to_ccyymmdd(ivd)>dt2 then goto READ_TRALLOC ! DONT POST ANY INVOICE AS EXPENSE IF OUTSIDE POSTING DATE
18840     if include_prev_posted$="N" and gde=2 then goto READ_TRALLOC
18860     if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then 
18880       write #work,using 'Form POS 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,ivd,ltrm$(iv$)(1:8),vn$,de$,amt, 0, 4, 0
18900     else 
18920       write #work,using 'Form POS 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,ivd,ck$,vn$,de$,amt,0,tcde,scd
18940     end if 
18960     gde=2
18980     goto L1020
19000 L990: write #work,using 'Form POS 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,ivd,ck$,vn$,de$,amt,bank_code,tcde,scd
19020 L1000: if pcde=0 or pcde=2 then pcde+=1
19040     if gde=0 or gde=2 then gde+=1
19060 L1020: ! 
19080     if glt=glt_post then 
19100       rewrite #tralloc,using 'Form POS 80,N 1': gde
19120     end if 
19140     goto READ_TRALLOC
19160 L1040: ! 
19180     if glt=glt_post then 
19200       rewrite #trmstr,using 'Form POS 71,N 1': pcde
19220     end if 
19240     if scd=4 then gosub PRDWRITE
19260     goto L630
19280 END2: close #1: 
19300     close #tralloc: 
19320     if lrec(work)=0 then goto ENDALL
19340     close #work: 
19360     open #1: "Name="&env$('Temp')&"\CONTROL."&wsid$&",SIZE=0,RecL=128,Replace",internal,output 
19380     write #1,using L1150: "! SORT FOR G/L DISTRIBUTION LIST IN PROCESS"
19400     write #1,using L1150: "FILE "&env$('Temp')&"\WORK."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
19420     write #1,using L1150: "MASK 1,26,C,A"
19440 L1150: form pos 1,c 128
19460     close #1: 
19480     fnFree(env$('Temp')&"\Addr."&session$)
19500     execute "SORT "&env$('Temp')&"\CONTROL."&wsid$&" -n"
19520     open #1: "Name="&env$('Temp')&"\Addr."&session$,internal,input ioerr ENDALL
19540     open #work=5: "Name="&env$('Temp')&"\WORK."&session$,internal,input,relative 
19560     if pr1$="N" then goto L1240
19580     if f1=0 then gosub HDR
19600     pr #255: "____________  ________  ________  ________  Regular GL Postings___________  __________  __________" pageoflow NEWPGE
19620 L1240: ! 
19630     read #1,using 'Form POS 1,PD 3': r5 eof ENDALL
19640     read #work,using 'Form POS 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1',rec=r5: gl$,ivd,ck$,vn$,de$,amt,bank_code,tcde,scd noRec L1240
19660     if amt=0 then goto L1240
19680     if gl$(1:3)="  0" then gl$(1:3)="   "
19700     if gl$(10:12)="  0" then gl$(10:12)="   "
19720     if hgl$=gl$ then goto L1410
19740     if prc$="N" then goto L1350
19760     if sc2><4 then goto L1350
19780     gosub PRGL
19800     if pr1$="Y" then 
19820       pr #255,using 'Form POS 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,2*C 10,C 30,2*N 12.2': pgl$,pivd,"  ","  ",pde$,pa1,pa2 pageoflow NEWPGE
19840     end if 
19860     pa1=pa2=sc2=0
19880 L1350: ! 
19900     if tc1=0 and tc2=0 then goto L1410
19920     if pr1$="N" then goto L1400
19940     pr #255: "                                            ______________________________  __________  __________" pageoflow NEWPGE
19960     pr #255,using 'Form POS 45,C 30,2*N 12.2': "GL # "&hgl$&" TOTAL",tc1,tc2
19980     pr #255: "                                            ______________________________  __________  __________" pageoflow NEWPGE
20000 L1400: ! 
20020     tc1=tc2=0
20040 L1410: ! 
20060     p1=75 : cl=1
20080     if tcde=1 and amt<0 then p1=87 : cl=2
20100     if tcde=2 and amt>0 then p1=87 : cl=2
20120     if tcde=3 and amt>0 then p1=87 : cl=2
20140     if tcde=4 and amt<0 then p1=87 : cl=2
20160     if prc$="N" then goto L1500
20180     gosub COMBINEPR
20200 ! aMT=ABS(AMT)
20220     if scd=4 then goto L1530
20240 L1500: ! aMT=ABS(AMT)
20260     gosub REGGL
20280     if pr1$="Y" then pr #255,using L1530: gl$,ivd,ck$,vn$,de$,abs(amt) pageoflow NEWPGE
20300 L1530: form pos 1,c 14,pic(zz/zz/zz),x 2,2*c 10,c 30,pos p1,g 12.2,skip 1
20320     if cl=2 then 
20340       tc2=tc2+abs(amt)
20360       gc2=gc2+abs(amt)
20380     else 
20400       tc1=tc1+abs(amt)
20420       gc1=gc1+abs(amt)
20440     end if 
20460     if tcde<>3 then goto L1590
20480     p1=75
20500     if pr1$="Y" then 
20520       pr #255,using L1530: bgl$,ivd,ck$,vn$,de$,abs(amt) pageoflow NEWPGE
20540     end if 
20560     if cl<>2 then 
20580       tc2=tc2+abs(amt)
20600       gc2=gc2+abs(amt)
20620     else 
20640       tc1=tc1+abs(amt)
20660       gc1=gc1+abs(amt)
20680     end if 
20700 L1590: ! 
20720     if bank_code=0 then goto L1660
20740     if tcde=1 then tbc(bank_code,1)=tbc(bank_code,1)+amt
20760     if tcde=2 then tbc(bank_code,2)=tbc(bank_code,2)+amt
20780 ! IF TCDE=3 AND AMT>0 THEN tBC(Bank_Code,2)=TBC(Bank_Code,2)+AMT
20800 ! IF TCDE=3 AND AMT<0 THEN tBC(Bank_Code,1)=TBC(Bank_Code,1)+ABS(AMT)
20820     if tcde=4 then tbc(bank_code,1)=tbc(bank_code,1)+amt
20840     goto L1740
20860 L1660: ! 
20880     ap1=val(gl$(1:3))
20900     if ap1=0 then j=99 : goto L1720
20920     for j=1 to 98
20940       if apc(j,1)=0 then goto L1720
20960       if apc(j,1)=ap1 then goto L1720
20980     next j
21000 L1720: ! 
21020     apc(j,1)=ap1
21040     apc(j,2)=apc(j,2)+(amt)
21060 L1740: ! 
21080     hgl$=gl$
21100     goto L1240 ! /r
22000 NEWPGE: ! r:
22020     pr #255: newpage
22040     gosub HDR
22060     continue  ! /r
24000 HDR: ! r:
24020     pg=pg+1
24040     f1=1
24060     pr #255,using 'form pos 1,c 8,cc 76': date$,env$('cnam')
24080     pr #255,using 'form pos 1,c 8,cc 76': time$,"General Ledger Distribution Listing"
24100     pr #255,using 'form pos 1,c 4,n 4,cc 76': "Page",pg,"From: "&cnvrt$("PIC(zz/ZZ/ZZ)",d1)&"   To: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)
24120     pr #255: ""
24140     pr #255: "                        Check/Ref                                          "
24160     pr #255: "  GL Number     Date     Number   Vendor #  Description                       Debits      Credits"
24180     pr #255: "____________  ________  ________  ________  ______________________________  __________  __________" pageoflow NEWPGE
24200     return  ! /r
26000 ENDALL: ! r:
26020     endall=1 : pr1$="Y" ! pr TOTALS
26040     if sc2=4 then gosub PRGL
26060     if pr1$<>"N" then 
26080       if sc2=4 then 
26100         pr #255,using 'Form POS 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,2*C 10,C 30,2*N 12.2': pgl$,pivd," "," ",pde$,pa1,pa2 pageoflow NEWPGE
26120       end if 
26140       pr #255: "                                            ______________________________  __________  __________" pageoflow NEWPGE
26160       pr #255,using 'Form POS 45,C 30,2*N 12.2': "GL # "&hgl$&" Total",tc1,tc2
26180       pr #255: "                                            ______________________________  __________  __________"
26200     end if 
26220     for j=1 to 99
26240       gl$=""
26260       if tbc(j,1)=0 and tbc(j,2)=0 then goto L2130
26280       read #bankmstr,using 'Form POS 33,C 12', key=lpad$(str$(j),2): gl$ nokey L2100
26300       if gl$(1:3)="  0" then gl$(1:3)="   "
26320       if gl$(10:12)="  0" then gl$(10:12)="   "
26340 L2100: ! 
26360       gosub BANKGL
26380       if pr1$="Y" then 
26400         pr #255,using 'Form POS 45,C 30,2*N 12.2': "Bank   "&gl$,tbc(j,2),tbc(j,1) pageoflow NEWPGE
26420       end if 
26440       gc1=gc1+tbc(j,2): gc2=gc2+tbc(j,1)
26460 L2130: ! 
26480     next j
26500     if pr1$="Y" then 
26520       pr #255: "                                            ______________________________  __________  __________" pageoflow NEWPGE
26540     end if 
26560     for j=1 to 99
26580       gl$=""
26600       if apc(j,2)=0 and apc(j,3)=0 then goto L2240
26620       read #fundmstr,using 'Form Pos 52,C 12',key=lpad$(str$(apc(j,1)),3): gl$ nokey L2210
26640       if gl$(1:3)="  0" then gl$(1:3)="   "
26660       if gl$(10:12)="  0" then gl$(10:12)="   "
26680 L2210: ! 
26700       gosub APGL
26720       if pr1$="Y" then 
26740         pr #255,using 'Form POS 45,C 30,2*N 12.2': "A/P    "&gl$,apc(j,3),apc(j,2) pageoflow NEWPGE
26760       end if 
26780       gc1=gc1+apc(j,3)
26800       gc2=gc2+apc(j,2)
26820 L2240: ! 
26840     next j
26860     if pr1$="N" then goto L2300
26880     pr #255: "                                            ______________________________  __________  __________" pageoflow NEWPGE
26900     pr #255,using 'Form POS 45,C 30,2*N 12.2': "Final Total",gc1,gc2 pageoflow NEWPGE
26920     pr #255: "                                            ======================================================" pageoflow NEWPGE
26940     fncloseprn
26960 L2300: ! 
26980     if glt=glt_print_only then goto XIT
27000     close #20: ioerr ignore
27020     open #20: "Name="&env$('Q')&"\CLmstr\PostDat.H"&env$('cno')&",Replace,RecL=12",internal,outIn,relative 
27040     write #20,using 'Form POS 1,2*N 6',rec=1: d1,d2
27060     close #20: 
27080     if glb=2 then 
27100       goto XIT
27120     else 
27140       fnputcno(gl2)
27180       fnchain("S:\acsGL\ACGLMrge")
27200     end if 
27220 ! /r
29000 END1: ! r:
29010     if scd=4 and pa1+pa2<>0 then gosub COMBINEPR
29020     if up1$="C" then goto END2
29040     open #paytrans=6: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
29060     open #unpdaloc=7: "Name="&env$('Q')&"\CLmstr\UnPdAloc.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Uaidx2.H"&env$('cno')&",Shr",internal,outIn,keyed 
29080     open #paymstr=8: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&env$('cno')&",Shr",internal,input,keyed 
29100 READ_PAYTRANS: ! 
29120     read #paytrans,using 'Form POS 1,C 8,C 12,N 6,POS 45,C 18,POS 96,N 1,N 6': vn$,iv$,dd,de$,pcde,pdte eof L2610
29140     if include_prev_posted$="Y" then goto L2450
29160     if pcde=2 then goto READ_PAYTRANS
29180 L2450: ! 
29200     if include_prev_posted><1 then goto L2500
29220     if pdte=0 then goto L2500
29240     if fndate_mmddyy_to_ccyymmdd(pdte)<dt1 then goto READ_PAYTRANS
29260     if fndate_mmddyy_to_ccyymmdd(pdte)>dt2 then goto READ_PAYTRANS
29280     goto L2510
29300 L2500: ! 
29320     if fndate_mmddyy_to_ccyymmdd(dd)>dt2 then goto READ_PAYTRANS
29340 L2510: ! 
29360     read #paymstr,using 'Form POS 9,C 30',key=vn$: de$ nokey L2520
29380 L2520: ! 
29400     restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
29420 L2540: ! 
29440     read #unpdaloc,using 'Form POS 1,c 8,c 12,C 12,PD 5.2': trvn$,triv$,gl$,amt eof READ_PAYTRANS
29460     if vn$<>trvn$ or iv$<>triv$ then goto READ_PAYTRANS
29480     if amt=0 then goto L2580
29500     write #work,using 'Form POS 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,dd,ltrm$(iv$)(1:8),vn$,de$,amt,0,4,0
29520 L2580: ! 
29540     if glt=glt_post then 
29560       rewrite #paytrans,using 'Form POS 96,N 1,N 6': 2,d2 ioerr L2590
29580     end if 
29600 L2590: ! 
29620     goto L2540
29640 L2610: ! 
29660     close #paytrans: 
29680     close #unpdaloc: 
29700     close #paymstr: 
29720     goto END2 ! /r
31000 COMBINEPR: ! r:
31020     if scd><4 then goto L2750
31040     if pgl$=gl$ and pivd=ivd then goto L2730
31060     if sc2><4 then goto L2720
31080     gosub PRGL
31100     if pr1$="Y" then 
31120       pr #255,using 'Form POS 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,2*C 10,C 30,2*N 12.2': pgl$,pivd,"  ","  ",pde$,pa1,pa2 pageoflow NEWPGE
31140     end if 
31160 L2720: ! 
31180     pa1=pa2=0
31200 L2730: ! 
31220     sc2=scd : pgl$=gl$ : pivd=ivd : pde$="Payroll Total" : pbank_code=bank_code
31240     if amt<0 then pa2+=abs(amt) else pa1+=amt
31260 L2750: ! 
31280     return  ! /r
33000 GLBUCKET_STUFF: ! r:
33020     if glt=glt_print_only then goto L2840
33040     d2$=cnvrt$("PIC(######)",d2)
33060     open #glbucket=20: "Name="&env$('Q')&"\GLmstr\GLBucket.H"&str$(gl2),internal,input,relative ioerr L2830
33080     read #glbucket,using 'Form POS 1,N 1',rec=1: glb noRec ignore
33100     close #glbucket: 
33120 L2830: ! 
33140     if glb=2 then 
33160       glwk$=env$('Q')&"\GLmstr\GL"&d2$&".H"&str$(gl2)
33180       open #glwk=11: "Name="&glwk$&",RecL=104,Use",internal,output 
33200     else 
33220       glwk$=env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&str$(gl2)
33240       open #glwk=11: "Name="&glwk$&",Size=0,RecL=104,Replace",internal,output 
33260     end if 
33280 L2840: ! 
33300     if pr2$<>"N" then 
33320       open #glwk2wsid=13: "Name="&env$('Q')&"\GLmstr\GLWK2"&wsid$&".H"&str$(gl2)&",RecL=110,Replace",internal,output 
33340     end if 
33360     return  ! /r
35000 REGGL: ! r:
35020     gw$=gl$ : wbank_code=bank_code
35040 REGGL2: ! 
35060     tr4=ivd : tr5=amt : tr6=tcde
35080     if tr6=2 or tr6=3 then tr5=-tr5
35100     if scd=4 then tr6=1
35120     tr$=ck$ : td$=de$ : ven$="" ! VN$
35140     if tr6=3 then 
35160       gosub SOMETHING
35180       tr5=-tr5
35200       gw$=bgl$
35220       wbank_code=bank_code
35230     end if 
35240     goto SOMETHING
35260 ! 
35280 PRGL: ! 
35300     gw$=pgl$ : tr4=pivd : x=pivd : pivd=x
35320     tr$="PR "&cnvrt$("PIC(ZZ/ZZ/ZZ)",pivd) : td$=pde$ : ven$="" : wbank_code=pbank_code
35340     if pa1<>0 then 
35360       tr5=pa1
35380       tr6=1
35400       gosub SOMETHING
35420     end if 
35440     if pa2=0 then 
35460       goto EO_SOMETHING
35480     else 
35500       tr5=-pa2 : tr6=1 : ven$=""
35520       goto SOMETHING
35540     end if 
35560 ! 
35580 BANKGL: ! 
35600     gw$=gl$ : wbank_code=bank_code : tr4=d2 : x=d2 : d2=x
35620     tr$="BK "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)
35640     td$="Bank #:"&str$(j)&" Total" : ven$=""
35660     if tbc(j,1)<>0 then 
35680       tr5=-tbc(j,1)
35700       tr6=1
35720       gosub SOMETHING
35740     end if 
35760     if tbc(j,2)=0 then 
35780       goto EO_SOMETHING
35800     else 
35820       tr5=tbc(j,2)
35840       tr6=2
35860       goto SOMETHING
35880     end if 
35900 ! 
35920 APGL: ! 
35940     gw$=gl$ : wbank_code=bank_code : tr4=d2 : x=d2 : d2=x
35960     tr$="AP "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2) : td$="AP Total"
35980     ven$=""
36000     if apc(j,2)<>0 then 
36020       tr5=-apc(j,2)
36040       tr6=4
36060       gosub SOMETHING
36080     end if 
36100     goto EO_SOMETHING
36120 ! 
37000 SOMETHING: ! 
37020     if tcde><4 then goto L3160
37040     if rtrm$(gw$(1:3))="" then 
37060       goto L3160
37080     else 
37100       read #fundmstr,using 'Form Pos 52,C 12',key=gw$(1:3): bgl$ nokey L3160
37120     end if 
37140     goto L3170
37160 L3160: ! 
37180     read #bankmstr,using 'Form POS 33,C 12', key=lpad$(str$(wbank_code),2): bgl$ nokey L3170
37200 L3170: ! 
37220     if glt=glt_post then 
37240       write #glwk,using 'Form Pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gw$,tr4,tr5,tr6,0,tr$,td$,"","","","",bgl$
37260     end if 
37280     gosub FUNDTR
37300 EO_SOMETHING: ! 
37320     return  ! /r
37800 XIT: ! 
37880   fnend 
39000 REVERSE_AP: ! r: Reverse AP Entries
39020   if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto L3470
39040   ap1=val(gl$(1:3))
39060   if ap1=0 then 
39080     j=99
39100     goto L3300
39120   end if 
39140   for j=1 to 98
39160     if apc(j,1)=0 then goto L3300
39180     if apc(j,1)=ap1 then goto L3300
39200   next j
39220 L3300: ! 
39240   apc(j,1)=ap1
39260   apc(j,3)+=amt
39280   p1=75 : gw$=""
39300   read #fundmstr,using 'Form Pos 52,C 12',key=lpad$(str$(ap1),3): gw$ nokey L3350
39320   goto L3360
39340 L3350: ! 
39360   read #fundmstr,using 'Form Pos 52,C 12',key=lpad$(str$(0),3): gw$ nokey L3360
39380 L3360: ! 
39400   gosub REGGL2
39420   tbc(bank_code,1)=tbc(bank_code,1)+amt
39440   if pr1$="N" then goto L3470
39460   if f1=0 then gosub HDR
39480   if ap2=0 then 
39500     pr #255: "____________  ________  ________  Reduce Accounts Payable for Previously Posted Invoices  ________"
39520     ap2=1
39540   end if 
39560   p1=75 : gw$=""
39580   read #fundmstr,using 'Form Pos 52,C 12',key=lpad$(str$(ap1),3): gw$ nokey L3430
39600 L3430: ! 
39620   pr #255,using 'Form POS 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 12,C 8,C 30,POS P1,N 12.2': gw$,pd,ck$,"","Reverse AP",amt pageoflow NEWPGE
39640   p1=87: gw$=""
39660   read #bankmstr,using 'Form POS 33,C 12', key=lpad$(str$(bank_code),2): gw$ nokey L3460
39680 L3460: ! 
39700   pr #255,using 'Form POS 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 12,C 8,C 30,POS P1,N 12.2': gw$,pd,ck$,"","Take Out of Bank",amt pageoflow NEWPGE
39720 L3470: ! 
39740   return  ! /r
41000 PRDBLD: ! r:
41020   if pr2$="N" then goto L3770
41040   tr5=amt
41060   if ivd=1 then prd(4)+=tr5
41080   if ivd>1 and ivd<5 then prd(ivd+3)=-tr5
41100   if ivd=15 then prd(8)=-tr5
41120   if ivd>4 and ivd<15 and dedcode(ivd-4)=2 then tr5=-tr5
41140   if ivd>4 and ivd<15 then prd(ivd+4)=-tr5
41160   if ivd=16 then prd(19)=-tr5
41180   if fp(ivd*.01)=.19 then prd(20)=int(ivd*.01)
41200 L3770: ! 
41220   return  ! /r
42000 PRDWRITE: ! r:
42020   if pr2$="N" then goto L3850
42040   prd(1)=val(vn$) conv L3850
42060   prd(2)=pd
42080   prd(3)=val(ck$) conv L3830
42100 L3830: ! 
42120   prd(22)=ca1
42140   write #glwk2wsid,using 'Form POS 1,N 4,2*PD 4,19*PD 5.2,PD 3': mat prd
42160 L3850: ! 
42180   mat prd=(0)
42200   return  ! /r
43000 FUNDTR: ! r: CREATE FUND TRANSFERS
43020   if endall=1 then goto EO_FUNDTR
43040   if uprc$(gw$(1:3))=uprc$(bgl$(1:3)) then goto EO_FUNDTR
43060   if val(gw$(1:3))=0 then goto EO_FUNDTR
43080   td$="Fund Transfer"
43100   gl1$=gl2$=gl3$="  0     0  0"
43120   read #fundmstr,using L4000,key=gw$(1:3): gl1$,gl2$,gl3$ nokey EO_FUNDTR
43140   gl1$=lpad$(rtrm$(gl1$),9)
43160   gl2$=lpad$(rtrm$(gl2$),9)
43180   if val(gl1$(1:3))=0 and val(gl1$(4:9))=0 and val(gl1$(10:12))=0 then 
43200     goto EO_FUNDTR ! no interfund entries if no gl # in i/f file
43220   else if val(gl2$(1:3))=0 and val(gl2$(4:9))=0 and val(gl2$(10:12))=0 then 
43260     goto EO_FUNDTR ! no interfund entries if no gl # in i/f file
43280   end if 
43300   read #fundmstr,using 'Form Pos 52,C 12',key=bgl$(1:3): bankgl3$ nokey EO_FUNDTR
43320   if gde>1 and gl3$=bankgl3$ then goto EO_FUNDTR ! Skip as check if previously posted interfund transfers in the Unpaid Invoice File (will post in unpaid file if AP numbers same in fund file
43340 L4000: form pos 34,2*c 9,c 12
43360   if glt><1 then goto L4040
43380   write #glwk,using 'Form Pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gw$(1:3)&gl1$,tr4,-tr5,tr6,0,tr$,td$,"","","","",bgl$
43400   write #glwk,using 'Form Pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$(1:3)&gl2$,tr4,tr5,tr6,0,tr$,td$,"","","","",bgl$
43420 L4040: ! 
43440   if pr1$="Y" then 
43460     pr #255,using 'Form POS 1,X 14,C 60': "Transferred from "&gw$(1:3)&gl1$&" to "&bgl$(1:3)&gl2$&cnvrt$("N 10.2",tr5) pageoflow NEWPGE
43480   end if 
43500 EO_FUNDTR: ! 
43520   return  ! /r
45000 IGNORE: continue 
45020 ! <Updateable Region: ERTN-xitable$>
45040 ERTN: fnerror(program$,err,line,act$,xitable$)
45060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
45080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
45100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
45120 ERTN_EXEC_ACT: execute act$ : goto ERTN
45140 ! /region
47000   def fn_check_breakdowns_add_up ! 
47020     check_breakdowns_add_up_return=1
47040     if ~fn_cb_trmstr_test then check_breakdowns_add_up_return=0
47060     if ~fn_cb_unpaid_test then check_breakdowns_add_up_return=0
47080     fn_check_breakdowns_add_up=check_breakdowns_add_up_return
47100   fnend  ! 
48000   def fn_cb_unpaid_test ! CHECK_UNPAIDS: !
48020     cb_cu_return=1
48040     restore #trmstr: 
48060     open #paymstr=8: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&env$('cno')&",Shr",internal,input,keyed 
48080     open #unpdaloc=7: "Name="&env$('Q')&"\CLmstr\UnPdAloc.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Uaidx2.H"&env$('cno')&",Shr",internal,outIn,keyed 
48100     open #paytrans=6: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
48120 CB_CU_READ: ! 
48140     read #paytrans,using 'Form POS 1,C 8,C 12,N 6,POS 45,C 18,POS 96,N 1,N 6,pos 63,g 10.2': vn$,iv$,dd,de$,pcde,pdte,upa eof EO_PAYTRANS_TEST
48160     invalloc=0
48180     if include_prev_posted$="Y" then goto L4440
48200     if pcde=2 then goto CB_CU_READ
48220 L4440: ! If include_prev_posted><1 Then Goto 4490
48240 ! If PDTE=0 Then Goto 4400
48260     if fndate_mmddyy_to_ccyymmdd(dd)<dt1 then goto CB_CU_READ
48280     if fndate_mmddyy_to_ccyymmdd(dd)>dt2 then goto CB_CU_READ
48300     read #paymstr,using 'Form POS 9,C 30',key=vn$: de$ nokey CB_CU_READ
48320     restore #unpdaloc,key>=vn$&iv$: nokey CB_CU_READ
48340     do 
48360       read #unpdaloc,using 'Form POS 1,c 8,c 12,C 12,PD 5.2': trvn$,triv$,gl$,amt eof CB_CU_FINIS
48380       if vn$<>trvn$ or iv$<>triv$ then goto CB_CU_FINIS
48400       invalloc+=amt
48420     loop 
48440 CB_CU_FINIS: ! 
48460     if upa<>invalloc then 
48480       mat ml$(3)
48500       ml$(1)="The allocations ("&trim$(cnvrt$("pic(---,---.##)",invalloc))&") does not match the total"
48520       ml$(2)="transaction amount ("&trim$(cnvrt$("pic(---,---.##)",upa))&").  You must fix this unpaid "
48540       ml$(3)="invoice # "&trim$(x$)&" in the unpaid invoice file before you can continue. "
48560       fnmsgbox(mat ml$,ok$,cap$,48)
48580       cb_cu_return=0
48600       goto EO_PAYTRANS_TEST
48620     else 
48640       goto CB_CU_READ
48660     end if 
48680 EO_PAYTRANS_TEST: ! 
48700     fn_cb_unpaid_test=cb_cu_return
48720     close #paymstr: 
48740     close #paytrans: 
48760     close #unpdaloc: 
48780   fnend 
52000   def fn_cb_trmstr_test ! TEST_CHECKHISTORY: !
52020     cb_tt_return=1
52040     do 
52060       totalloc=0
52080 CB_TT_READ: ! 
52100       read #trmstr,using 'Form POS 1,n 2,n 1,C 8,N 6,PD 10.2,POS 28,C 8,C 30,POS 71,N 1,X 6,N 1': trbank_code,trtcde,ck$,pd,ca1,vn$,de$,pcde,scd eof EO_TRMSTR_TEST
52120       if scd=4 and fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto CB_TT_READ
52140       if fndate_mmddyy_to_ccyymmdd(pd)<dt1 then goto CB_TT_READ
52160       restore #tralloc,key>=cnvrt$("pic(zz)",trbank_code)&cnvrt$("pic(#)",trtcde)&ck$: nokey CB_TT_READ
52180       do  ! CB_TT_READ_TRALLOC: !
52190         read #tralloc,using 'Form POS 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,C 6,POS 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd$,gde eof CB_TT_FINIS ! eof EO_TRMSTR_TEST
52192         ivd=val(ivd$) conv ignore ! ivd$ logic added 8/12/2015 to prevent merriam wood's error here from using characters in this field
52200         if up1$="C" and gde=2 then gde=1 ! don't allow old accrual codes mess up cash basis
52220         if ivd=0 then ivd=pd ! kJ   10/02/06   skipping receipts w/o invoice numbers
52240         if bank_code<>trbank_code or trtcde<>tcde or ck$<>trck$ then goto CB_TT_FINIS ! thru, allocation doesn/t belong to this transaction
52260         totalloc+=amt
52280       loop  !  goto CB_TT_READ_TRALLOC
52300 CB_TT_FINIS: ! 
52320       if ca1<>totalloc then 
52340         mat ml$(3)
52360         ml$(1)="The allocations ("&cnvrt$("pic(---,---.##)",totalloc)&" does not match the total"
52380         ml$(2)="transaction amount ("&cnvrt$("pic(---,---.##)",ca1)&".  You must fix this "
52400         ml$(3)="transaction ("&ck$&") (bank "&str$(trbank_code)&") check history before you can continue. "
52420         fnmsgbox(mat ml$,ok$,cap$,48)
52440         cb_tt_return=0
52460         goto EO_TRMSTR_TEST
52480       end if 
52500     loop 
52540 EO_TRMSTR_TEST: ! 
52560     fn_cb_trmstr_test=cb_tt_return
52580   fnend 
