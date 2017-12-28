00010 ! S:\acsCL\unpaidinvoice - backup copy in C:\ACS\Dev-5\acsCL\UnpaidInvoice(before_overhaul09-28-2016).br.brs
02000 ! r: SETUP: fntop, dims, open files, etc
02020   library 'S:\Core\Library': fntop,fnxit, fncno,fnopenprn,fncloseprn,fnerror,fnTos,fnFra,fnLbl,fnTxt,fncombof,fncomboa,fnButton,fnCmdKey,fnAcs,fnmsgbox,fnflexadd1,fnflexinit1,fnChk,fnaddpayee,fnagl$,fnrgl$,fnjob_srch,fncmbjob,fncmbcategory
02040   library 'S:\Core\Library': fngethandle,fncmbsubcat,fncategory_srch,fnregistered_for_job_cost_pr,fnCmdSet,fnrglbig$,fnqglbig
02060   fntop(program$,cap$="Unpaid Invoice")
02080   on error goto ERTN
02120 ! ______________________________________________________________________
02140   dim cap$*128
02160   dim jobdesc$*30,jn$*6,l(11),ta(2),jobname$*25,jobitem$(6)*30
02180   dim in1$(9),de$*30,ta(2)
02200   dim pr$(4)*30,t1(5),up$(4),unpaidkey$*20
02220   dim d(2),sn$*50
02240   dim jn$*6,cn$*11,l(13)
02260   dim contact$*30,ph$*12,email$*50,fax$*12,myact$*20,resp$(50)*50
02280   dim chdr$(16),cmask$(16),item$(16)*21 ! used with flex grid
02300   dim gldesc$*30,ml$(3)*80
02320   dim item1$(3)*15,type$*25,holdkey$*20,resp$(256)*50 ! holdresp$(256)*50,
02340 ! ______________________________________________________________________
02360 ! screen_last=5
02380 ! ______________________________________________________________________
02400   right=1
02420   fncno(cno)
02440   open #20: "Name=Q:\CLmstr\PostDat.h"&env$('cno')&",Shr,Use,RecL=12",internal,outIn,relative 
02460   read #20,using 'Form POS 1,2*N 6',rec=1: dt1,dt2 noRec L690
02480   goto L700
02500 L690: ! 
02520   write #20,using 'Form POS 1,2*N 6',rec=1: dt1,dt2
02540 L700: ! 
02560   close #20: 
02580 ! 
02600   open #20: "Name=Q:\CLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative 
02620   read #20,using 'Form POS 150,2*N 1,C 2',rec=1: mat d,bc$
02640   if d(1)=0 and d(2)=0 then 
02660     glnmask$='50'
02680   else if d(1)=1 and d(2)=0 then 
02700     glnmask$='51'
02720   else if d(1)=0 and d(2)=1 then 
02740     glnmask$='52'
02760   else if d(1)=1 and d(2)=1 then 
02780     glnmask$='53'
02800   end if 
02820   close #20: 
02840 ! 
02860   bankcode=val(bc$)
02880   open #bankmstr=fngethandle: "Name=Q:\CLmstr\BankMstr.H"&env$('cno')&",KFName=Q:\CLmstr\BankIdx1.H"&env$('cno')&",Shr",internal, outin, keyed 
02900   read #bankmstr,using 'Form POS 45,PD 6.2,PD 6.2',key=bc$,release: bal,upi nokey ignore
02920   close #bankmstr: 
02940   open #glmstr=fngethandle: "Name=Q:\CLmstr\GLmstr.H"&env$('cno')&",KFName=Q:\CLmstr\GLIndex.H"&env$('cno')&",Shr",internal,outIn,keyed 
02960   open #paymstr1=13: "Name=Q:\CLmstr\PayMstr.H"&env$('cno')&",KFName=Q:\CLmstr\PayIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
02980   open #paymstr2=14: "Name=Q:\CLmstr\PayMstr.H"&env$('cno')&",KFName=Q:\CLmstr\PayIdx2.H"&env$('cno')&",Shr",internal,outIn,keyed 
03000   open #payeegl=17: "Name=Q:\CLmstr\payeeGLBreakdown.h"&env$('cno')&",KFName=Q:\CLmstr\Payeeglbkdidx.h"&env$('cno')&",Shr",internal,outIn,keyed 
03020   open #paytrans=4: "Name=Q:\CLmstr\PayTrans.H"&env$('cno')&",KFName=Q:\CLmstr\UnPdIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
03040   open #unpdaloc=5: "Name=Q:\CLmstr\UnPdAloc.H"&env$('cno')&",KFName=Q:\CLmstr\Uaidx2.H"&env$('cno')&",Shr",internal,outIn,keyed 
03060   t1(1)=bal : upi=t1(5) : t1(3)=t1(1)-t1(2)
03080   if fnregistered_for_job_cost_pr then havejc=1 : gosub JCBLD
03100   goto MENU1 ! /r
06000 MENU1: ! r:
06020   mat chdr$(16) : mat cmask$(16) : mat item$(16)
06040   chdr$(1)='Ref': chdr$(2)='Payee': chdr$(3)='Invoice'
06060   chdr$(4)='Date'
06080   chdr$(5)='Due Date' : chdr$(6)='P O #'
06100   chdr$(7)='Description' : chdr$(8)='Amount'
06120   chdr$(9)='Disc Amt' : chdr$(10)='Disc Date'
06140   chdr$(11)='Pay Code' : chdr$(12)='Bank'
06160   chdr$(13)='Ck Num' : chdr$(14)='Date Paid'
06180   chdr$(15)='Post Code' : chdr$(16)='Post Date'
06200   cmask$(1)="30"
06220   cmask$(2)="": cmask$(3)="" : cmask$(4)='1'
06240   cmask$(5)='1' : cmask$(6)='': cmask$(7)=''
06260   cmask$(8)='10' : cmask$(9)='10' : cmask$(10)='1'
06280   cmask$(11)='30': cmask$(12)='30'
06300   cmask$(13)='30': cmask$(14)='3'
06320   cmask$(15)='30': cmask$(16)='1'
06340 DISPLAY_INVOICE_GRID: ! 
06360   fnTos(sn$="unpaid1")
06380   respc=0
06400   frame=0
06420   fnflexinit1('UnpaidFile',1,1,20,85,mat chdr$,mat cmask$,1,0)
06440   restore #paytrans: 
06460 READ_INVOICE_GRID: ! r: read unpaid invoice file and populate the grid
06480   read #paytrans,using 'Form POS 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof EO_INVOICE_GRID noRec L970
06500   item$(1)=str$(rec(paytrans))
06520   item$(2)=vn$: item$(3)=iv$: item$(4)=up$(1)
06540   item$(5)=up$(2) : item$(6)=up$(3) : item$(7)=up$(4)
06560   item$(8)=str$(upa) : item$(9)=str$(disamt) : item$(10)=str$(ddate)
06580   item$(11)=str$(pcde) : item$(12)=str$(bcde)
06600   item$(13)=str$(ckn) : item$(14)=str$(dp)
06620   item$(15)=str$(gde) : item$(16)=str$(pdte)
06640   fnflexadd1(mat item$)
06660 ! transactionstotal+=upa
06680 L970: ! 
06700   goto READ_INVOICE_GRID
06720 EO_INVOICE_GRID: ! /r
06740   if havejc=1 then 
06760     fnCmdKey("&Review Job Cost Entries",9,0,0,"Allows you to review and/or post any job cost allocations you have made.")
06780   end if 
06800   fnCmdKey("&Add",1,0,0,"Allows you to add new unpaid invoice records.")
06820   fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing unpaid invoice record.")
06840   fnCmdKey("&Select to Pay",8,0,0,"Allows you to code invoices for payment")
06860   fnCmdKey("&Listing",3,0,0,"Prints listings from unpaid file")
06880   fnCmdKey("E&xit",5,0,1,"Exits to main menu")
06900   fnAcs(sn$,0,mat resp$,ck)
06920   displayalljobs=0
06940   if ck=5 then goto FINIS
06960 ! screen=0
06980   if ck=2 then edit=1 : recordnumbertoedit=val(resp$(1)) else edit=0 : recordnumbertoedit=0
07000   if (ck=1 or ck=2) then let fn_addinvoice(vn$,iv$,recordnumbertoedit) : goto MENU1
07020   if ck=3 then gosub PRINTLISTING : goto DISPLAY_INVOICE_GRID ! print listings of unpaid invoice file
07040   if ck=8 then goto CODE_FOR_PAYMENT ! select invoices to payment
07060   if ck=9 then 
07080     displayalljobs=1
07100     jn$="" : iv$="" : vn$=""
07120     subcat=0 : cat=0
07140     gosub JOBCOST
07160     goto DISPLAY_INVOICE_GRID
07180   end if 
07200   pause 
07960 ! ! /r
08000 PRINTLISTING: ! r: print listings
08020 ! need screen here asking paid/unpaid/all,starting inv #,1st inv to print,show breakdowns(y/n),display last ref in file, 1st ref # used this time
08040 ! need ref # to begin print (blank for all)   rf2    show last ref  lrec(4)
08060   fnopenprn
08080   pg=0
08100   if rf2=0 then rf2=1
08120   gosub HDR
08140   t1=0
08160   for j=rf2 to lrec(4)
08180     read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,G 2,G 8,G 6,N 1',rec=j,release: mat in1$,ckn,dp,gde noRec NEXTRECORD
08200     if rtrm$(in1$(1))="" then goto NEXTRECORD
08220     t0=val(in1$(7)) conv L1440
08240     t1=t1+t0
08260 L1440: ! 
08280     print #255,using 'Form POS 1,N 4,X 2,C 10,C 14,2*C 8,C 14,C 20,C 12,C 6,G 4,N 10,N 8': j,mat in1$,ckn,dp pageoflow NEWPGE
08300     aa2=0
08320 !   r5=aa(1)
08340     restore #unpdaloc,key>=in1$(1)&"            ": nokey NEXTRECORD
08360 L1480: ! 
08380     read #unpdaloc,using 'Form POS 1,c 8,c 12,c 12,PD 5.2,C 30',release: vnkey$,vniv$,gl$,aa,de$ eof NEXTRECORD
08400     if in1$(1)<>vnkey$ then goto NEXTRECORD ! not same vendor
08420     if in1$(2)<>vniv$ then goto L1480 ! not same invoice
08440     aa2=aa2+aa
08460     print #255,using 'Form POS 47,c 12,X 2,C 20,N 10.2': gl$,de$(1:20),aa pageoflow NEWPGE
08480     goto L1480
08500     print #255: 
08520 NEXTRECORD: ! 
08540   next j
08560   print #255,using 'Form POS 81,C 10,SKIP 1,POS 81,N 10.2,SKIP 1,POS 81,C 10,SKIP 1': "__________",t1,"=========="
08580 ! 
08600   fncloseprn
08620   on fkey 99 ignore 
08640   return  ! /r
10000 NEWPGE: ! r:
10010   print #255: newpage
10020   gosub HDR
10030   continue  ! /r
12000 HDR: ! r:
12010   pg=pg+1
12020   fnopenprn
12030   print #255,using 'Form POS 1,C 8,Cc 82': date$,env$('cnam')
12040   print #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg,"Unpaid Invoice File Listing"
12050   print #255: ""
12060   print #255: "                             Invoice    Due     PO Number                                   Pay   Bank   Check     Date "
12070   print #255: "Ref#  Payee #   Invoice Numb   Date    Date     GL Number   Description            Amount   Code  Code   Number    Paid "
12080   print #255: "____  ________  ____________  ______  ______  ____________  __________________  __________  ____  ____  ________  ______"
12090   return  ! /r
12100 ! ______________________________________________________________________
14000 FINIS: ! r:
14010   if havejc=1 and lrec(jcbreakdown)>0 then 
14020     mat ml$(3)=("")
14030     ml$(1)="It appears you have "&str$(lrec(jcbreakdown))&"job cost entries"
14040     ml$(2)="that have not been posted.  Do you wish to post these"
14050     ml$(3)="entries before you exit?"
14060     fnmsgbox(mat ml$,resp$,cap$,4)
14070     if resp$="Yes" then gosub POST_TO_JOB
14080   end if 
14090   goto XIT ! /r
14100 XIT: fnxit
14110 ! ______________________________________________________________________
16000 JCBLD: ! r: Open JC Files
16010   mat chdr3$(6) : mat cmask3$(6) : mat jobitem$(6)
16020   chdr3$(1)='Refenence'
16030   chdr3$(2)='Job #'
16040   chdr3$(3)='Cat #'
16050   chdr3$(4)='Sub-Cat #'
16060   chdr3$(5)='Amount'
16070   chdr3$(6)='Description'
16080   cmask3$(5)='10' : cmask3$(1)=cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(6)=''
16090   open #41: "Name=Q:\PRmstr\JCMSTR.H"&env$('cno')&",KFName=Q:\PRmstr\JCIndx.H"&env$('cno')&",Shr",internal,outIn,keyed ioerr JCBLD_FINIS
16100   open #category:=2: "Name=Q:\PRmstr\JCCAT.H"&env$('cno')&", KFName=Q:\PRmstr\CATIndx.H"&env$('cno')&",Shr", internal,outIn,keyed 
16110   open #43: "Name=Q:\PRmstr\SCMSTR.H"&env$('cno')&",KFName=Q:\PRmstr\SCIndex.H"&env$('cno')&",Shr",internal,outIn,keyed 
16120   open #45: "Name=Q:\PRmstr\JCTrans.H"&env$('cno')&",Shr",internal,outIn,relative 
16130   if not exists("Q:\CLmstr\JCBreakdownS"&wsid$&".h"&env$('cno')) then gosub MAKE_JCB
16140   open #jcbreakdown=46: "Name=Q:\CLmstr\JCBreakdownS"&wsid$&".h"&env$('cno')&",KFName=Q:\CLmstr\JcBrkidx"&wsid$&".h"&env$('cno')&",Version=1,Shr",internal,outIn,keyed ioerr MAKE_JCB
16150 JCBLD_FINIS: ! 
16160   return  ! /r
18000 ! <Updateable Region: ERTN>
18010 ERTN: fnerror(program$,err,line,act$,"xit")
18020   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
18030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
18040   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
18050 ERTN_EXEC_ACT: execute act$ : goto ERTN
18060 ! /region
18070 IGNORE: continue 
18080 ! ______________________________________________________________________
21000 CODE_FOR_PAYMENT: ! r:
21010   lastrec=nextrec=total=0
21020   displayattop$="True"
21030   close #clearing: ioerr ignore
21040   open #clearing=fngethandle: "Name=Q:\CLmstr\clearing.H"&wsid$&",replace,RecL=114",internal,outIn,relative  ! kj wrong recl
21050   if displayunpaid=1 then 
21060     type$="Coded for Payment"
21070   else if displayunpaid=0 then 
21080     type$="Approved and Unapproved"
21090   else if displayunpaid=2 then 
21100     type$="Not Approved for Payment"
21110   end if 
21120   close #paytrans: ioerr ignore
21130   open #paytrans=4: "Name=Q:\CLmstr\PayTrans.H"&env$('cno')&",KFName=Q:\CLmstr\UnPdIdx1.H"&env$('cno')&",Shr",internal,outIn,keyed 
21140 L4700: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof DISPLAY_GRID
21150   if displayunpaid=1 and pcde=1 then goto L4760 ! if only choose selected, don't allow others to list
21160   if displayall=1 then goto L4760
21170   if displayunpaid=2 and pcde=0 then goto L4760 ! if only choose selected, don't allow others to list
21180   if displayunpaid<>0 then goto L4700 ! if displayed has an answer,but no match go back
21190 ! If PCDE<>0 Then Goto 14820  ! go back to read record in don't want selected invoices to show on grid  (not sure how to default)
21200 L4760: write #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
21210   if pcde=1 and displayunpaid<>2 then total+=upa ! total paids
21220   if pcde=0 and displayunpaid=2 then total+=upa ! total unpaids
21230   goto L4700
21240 DISPLAY_GRID: ! 
21250   mat chdr$(16) : mat cmask$(16) : mat flxitm$(16)
21260   chdr$(1)="Rec" : chdr$(2)="Bank" : chdr$(3)="Pay"
21270   chdr$(4)="Payee #" : chdr$(5)="Invoice #"
21280   chdr$(6)="Inv Date" : chdr$(7)="Due Date"
21290   chdr$(8)="Description": chdr$(9)="Amount"
21300   chdr$(10)="Dis Amt" : chdr$(11)="Dis Date"
21310   chdr$(12)="BK Code"
21320   chdr$(13)="Ck #" : chdr$(14)="D Paid"
21330   chdr$(15)="P C" : chdr$(16)="P Date"
21340   cmask$(1)='30' : cmask$(2)='30' : cmask$(3)=""
21350   cmask$(4)=''
21360   cmask$(5)='' : cmask$(6)='1': cmask$(7)='1'
21370   cmask$(8)='': cmask$(9)='10' : cmask$(10)='10'
21380   cmask$(11)='3' : cmask$(12)='30' : cmask$(13)='30'
21390   cmask$(14)='1' : cmask$(15)='30' : cmask$(16)="1"
21400 RE_DISPLAY_GRID: ! save a little time
21410   fnTos(sn$="paidinv")
21420   respc=0 : mat resp$=('')
21430   fnFra(2,1,13,23,"Approval Options"," ")
21440   fnButton(1,2,"&Approve All",62,"Will select to pay all unpaid invoices",1,18,1)
21450   fnButton(3,2,"&Approve by Range",63,"Enter a range of reference numbers to approve.  The reference # is the number to the left assigned by the computer.",1,18,1)
21460   fnLbl(4,4,"From:",5,1,0,1)
21470   fnTxt(4,11,5,0,1,"30",0,"Select the first reference # to be approved",1)
21480   resp$(respc_rangefrom:=respc+=1)=""
21490   fnLbl(5,4,"To:",5,1,0,1)
21500   fnTxt(5,11,5,0,1,"30",0,"Select the last reference # to be approved",1)
21510   resp$(respc_rangeto:=respc+=1)=""
21520   fnButton(7,2,"&Approve by Due Date",64,"Approve all invoices due by a certain date.",1,18,1)
21530   fnLbl(8,2,"Date:",5,1,0,1)
21540   fnTxt(8,8,8,0,1,"1",0,"All invoices with a due by date equal to or less than this date will be approved",1)
21550   resp$(respc_duedate:=respc+=1)=""
21570   fnButton(10,2,"Approve By Payee",66,"Approves all invoices with this payee number in invoice record.",1,18,1)
21580   fnLbl(11,2,"Payee #:",8,1,0,1)
21590   fnTxt(11,11,8,0,1,"",0,"Enter payee # to approve all invoices on that payee",1)
21600   resp$(respc_payee:=respc+=1)=""
21610   if displayunpaid=1 or displayunpaid=0 then 
21612     wording$="Total Selected:"
21614   else 
21616     wording$= "Total Unapproved:"
21618   end if 
21620   fnLbl(2,28,wording$,18,1)
21630   fnTxt(2,49,12,0,1,"10",0," ")
21640   resp$(respc_total:=respc+=1)=str$(total)
21650   fnChk(3,47,"Display at Top:",1)
21660   resp$(respc+=1)=displayattop$
21670   fnLbl(1,1,trim$(env$('cnam')(1:30))&"-"&type$,65,2)
21680   fnflexinit1('unpaidinv',5,27,15,55,mat chdr$,mat cmask$,1)
21682   respc_selectedrec=respc+=1
21690   restore #clearing: 
21700   if nextrec>0 and displayattop$="True" then goto L4890 else goto L5030
21710 L4890: for j=nextrec to lrec(clearing) ! read starting with next record
21720     read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L4940
21730     flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
21740     flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
21750     flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
21760     flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
21770     flxitm$(16)=str$(pdte)
21780     flxitm$(1)=str$(rec(clearing))
21790     if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
21800 fnflexadd1(mat flxitm$)
21810 L4940: next j
21820 if nextrec=1 then goto L5020 ! thinks it rereads the 1st record twice
21830 for j=1 to max(nextrec-1,1) ! read records previously coded or skipped
21840   read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
21850   flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
21860   flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
21870   flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
21880   flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
21890   flxitm$(1)=str$(rec(clearing))
21900   if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
21910 fnflexadd1(mat flxitm$)
21920 next j
21930 L5020: goto L5070
21940 L5030: ! 
21950 read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
21960 flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
21970 flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
21980 flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
21990 flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
22000 flxitm$(16)=str$(pdte)
22010 flxitm$(1)=str$(rec(clearing)) ! assign flxitm$(1) with new record #
22020 if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
22030 fnflexadd1(mat flxitm$) : goto L5030
22040 L5070: ! 
22050 fnCmdKey("&Approve Highlighted",1,1,0,"Approves or cancels the invoice that is highlighted.")
22060 fnCmdKey("&Display All",9,0,0,"Displays all remaining records in the unpaid file.")
22070 fnCmdKey("&Display Selected",3,0,0,"Displays all invoices selected for payment")
22080 fnCmdKey("&Display UnSelected",2,0,0,"Displays all remaining uncleared invoices")
22090 fnCmdKey("C&omplete",5,0,1,"Return to main unpaid invoice menu")
22100 fnAcs(sn$,0,mat resp$,ck)
22110 displayunpaid=total=displayall=0
22120 if ck=5 or ck=99 then goto MENU1
22130 selectedrec=val(resp$(respc_selectedrec)) ! selected record from grid
22140 rangefrom=val(resp$(respc_rangefrom)) ! if select range of reference numbers
22150 rangeto=val(resp$(respc_rangeto)) ! if select range of reference numbers
22160 duedate =val(resp$(respc_duedate)) ! used in selecting invoices by due date
22170 payeevn$=resp$(respc_payee) ! payee number to select
22180 total=val(resp$(respc_total)) ! total used for display only
22190 displayattop$=resp$(7) ! display at top
22200 if ck=2 then displayunpaid=2: goto CODE_FOR_PAYMENT !                                                   redisplay on uncoded
22210 if ck=3 then displayunpaid=1: goto CODE_FOR_PAYMENT ! displays only                                       cleared on this date
22220 if ck=9 then displayall=1: goto CODE_FOR_PAYMENT ! displays everything                                 in unpaid file
22230 if ck=62 then goto PAY_ALL
22240 if ck=63 and rangefrom=0 then goto MSGBOX3
22250 if ck=69 then goto APPROVE_BY_RANGE
22260 if ck=64 and duedate=0 then goto MSGBOX4
22270 if ck=64 then goto CLEAR_BY_DUEDATE
22280 if ck=65 then goto APPROVE ! approve or unselect an invoice
22290 if ck=66 then goto APPROVE_BY_PAYEE ! approve all invoices for a specific payee
22300 goto APPROVE ! /r  (used to just fall though to approve here)
23000 APPROVE: ! r: clear or unclear selected invoices
23010 read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=selectedrec: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
23020 if pcde=0 then pcde=1 : newbcde=bankcode : goto L5540 ! if no previous payment code, use new one; if it has a payment code, change it
23030 if pcde=1 and dp=0 then pcde=0 : newbcde=0: goto L5540 ! change from yes to no
23040 if pcde=0 then pcde=1 : newbcde= bankcode: goto L5540 ! change from no to yes
23050 if pcde=1 and dp>0 then pcde=1 : newbcde=bcde: goto L5540 ! don't change previously paid
23060 L5540: ! 
23070 if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
23080 ! Print PCDE,BCDE
23090 rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=selectedrec: pcde,newbcde
23100 rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,newbcde ! update the transaction history
23110 lastrec=selectedrec
23120 if lastrec+1 <= lrec(clearing) then nextrec=lastrec+1 else nextrec=1
23130 goto RE_DISPLAY_GRID ! /r
24000 MSGBOX3: ! r: need range of reference numbers
24010 mat ml$(2)
24020 ml$(1)="You must enter the 'Range From' and 'Range To'"
24030 ml$(2)="reference numbers to choose this option."
24040 fnmsgbox(mat ml$,resp$,cap$,16)
24050 goto CODE_FOR_PAYMENT ! /r
25000 MSGBOX4: ! r: need due date for selecting by due date
25010 mat ml$(2)
25020 ml$(1)="You must enter the 'Due Date' if you choose to'"
25030 ml$(2)="approve by due date."
25040 fnmsgbox(mat ml$,resp$,cap$,16)
25050 goto CODE_FOR_PAYMENT ! /r
26000 PAY_ALL: ! r: pay all unpaid invoices
26010 restore #paytrans: 
26020 L5710: ! 
26030 read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5760
26040 if bcde=0 then bcde=bankcode
26050 if pcde=0 then pcde=1: goto L5740
26060 goto L5710
26070 L5740: ! 
26080 rewrite #paytrans,using 'Form POS 73,n 1,n 2': pcde,bcde ! update the transaction history
26090 goto L5710
26100 L5760: ! 
26110 goto CODE_FOR_PAYMENT ! /r
27000 APPROVE_BY_RANGE: ! r: clear by reference # range
27010 for j=rangefrom to rangeto
27020   read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5860 noRec L5860
27030   if pcde>0 then goto L5850 ! already coded
27040   if pcde=0 then pcde=1
27050   if bcde=0 then bcde=bankcode ! don't change bank # if one                                                      previously entered
27060   rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,bcde ! update the transaction history
27070   rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=j: pcde,bcde ! update the transaction history
27080 L5850: ! 
27090 next j
27100 L5860: ! 
27110 goto CODE_FOR_PAYMENT ! /r
28000 CLEAR_BY_DUEDATE: ! r: clear any invoices with due date less than or equal the one entered
28010 for j=1 to lrec(clearing)
28020   read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5970 noRec L5970
28030   if val(up$(2))<=duedate then goto L5910 else goto L5960
28040 L5910: ! 
28050   if pcde>0 then goto L5960 ! already coded
28060   if pcde=0 then pcde=1
28070   if bcde=0 then bcde=bankcode ! don't change bank # if one previously entered
28080   rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,bcde ! update the transaction history
28090   rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=j: pcde,bcde ! update the transaction history
28100 L5960: ! 
28110 next j
28120 L5970: ! 
28130 goto CODE_FOR_PAYMENT ! /r
29000 APPROVE_BY_PAYEE: ! r: select payee to pay
29010 restore #paytrans: 
29020 L6000: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L6060 : lastrec=rec(paytrans)
29030 if uprc$(lpad$(rtrm$(payeevn$),8))<>uprc$(vn$) then goto L6000
29040 if pcde<>1 then pcde=1
29050 if bcde=0 then bcde=bankcode ! don't change bank # if one                                                      previously entered
29060 rewrite #paytrans,using 'Form POS 73,n 1,n 2',rec=lastrec: pcde,bcde ! update the transaction history
29070 goto L6000
29080 L6060: goto CODE_FOR_PAYMENT ! /r
30000 JOBCOST: ! r:
30010 dim jn$*6
30020 ENTRY_SCREEN: ! 
30030 fnTos(sn$="Jobcost")
30040 respc=0 : mat resp$=(''): lc=0: mylen=20: mypos=mylen+3
30050 fnLbl(lc+=1,1,"Payee # "&trim$(vn$)&" Invoice # "&trim$(iv$),50,0)
30060 fnLbl(lc+=2,1,"Job #:",mylen,1)
30070 ! fnTxt(LC,MYPOS,6,0,1,"",0,"Choose from the sub-category list.")
30080 ! .  !  rESP$(RESPC+=1)=JN$
30090 fncmbjob(lc,mypos)
30100 resp$(respc+=1)=jn$
30110 fnLbl(lc+=2,1,"Category #:",mylen,1)
30120 fncmbcategory(lc,mypos)
30130 resp$(respc+=1)=str$(cat)
30140 fnLbl(lc+=2,1,"Sub-category #:",mylen,1)
30150 fncmbsubcat(lc,mypos)
30160 resp$(respc+=1)=str$(subcat)
30170 fnLbl(lc+=1,1,"Amount:",mylen,1)
30180 fnTxt(lc,mypos,12,0,1,"10",0,"Enter the amount allocated to this category.")
30190 resp$(respc+=1)=str$(amt)
30200 fnLbl(lc+=1,1,"Description:",mylen,1)
30210 fnTxt(lc,mypos,25,0,0,"",0,"Enter the descritpion for the allocation.")
30220 resp$(respc+=1)=jobdesc$
30230 ! Job Cost Invoice Breakdown Grid
30240 fnflexinit1('JobAlloc',11,1,6,60,mat chdr3$,mat cmask3$,1,0,0)
30250 if displayalljobs=1 then restore #jcbreakdown: : goto L6270
30260 restore #jcbreakdown,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey EO_FLEX1
30270 L6270: totalcost=0: mat jobitem$=("")
30280 READ_JOB_ALLOCATIONS: ! 
30290 read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,holdvn$,holdiv$ eof EO_FLEX1
30300 if displayalljobs=1 then goto L6320 ! allow all entries to print
30310 if holdvn$<>vn$ or holdiv$<>iv$ then goto EO_FLEX1
30320 L6320: ! 
30330 totalcost+=amt
30340 jobitem$(1)=str$(rec(jcbreakdown)) : jobitem$(2)=jn$
30350 jobitem$(3)=str$(cat) : jobitem$(4)=str$(subcat)
30360 jobitem$(5)=str$(amt) : jobitem$(6)=jobdesc$
30370 fnflexadd1(mat jobitem$)
30380 goto READ_JOB_ALLOCATIONS
30390 EO_FLEX1: ! 
30400 fnButton(3,70,"&Search",68,"Will search for job numbers",1,9)
30410 fnButton(5,70,"&Search",69,"Will search for available category codes",1,9)
30420 fnLbl(18,18,"Total: "&trim$(cnvrt$("pic($$$,$$$,$$$.##)",totalcost)),22,right,0)
30430 if displayalljobs=0 then let fnLbl(19,18,"Invoice: "&trim$(cnvrt$("pic($$$,$$$,$$$.##)",upa)),22,right,0)
30440 fnButton(18,53,"&Edit",65,"Will allow you to change an allocation",1,5)
30450 fnButton(3,70,"&Search",63,"Will search for available category codes",1,9)
30460 fnCmdKey("&Next",1,1,0,"Accept this transaction)")
30470 fnCmdKey("&Listing",4,0,0,"Print listing of all job cost entries.")
30480 fnCmdKey("&Post To Jobs",3,0,0,"Post this batch ofjob cost entries to job cost records. Normally done once complete with batch.")
30490 fnCmdKey("&Cancel",5,0,1,"Cancels without posting to jub cost)")
30500 fnAcs(sn$,0,mat resp$,ck)
30510 if ck=4 then gosub PRINT_JOB_COST_ENTRIES: goto ENTRY_SCREEN
30520 if val(resp$(4))=0 and ck<>65 then ck=5 ! exit if no amount on next
30530 if ck=5 then amt=0: totalcost=0 : goto L6930 ! sCREEN=0: Goto MENU1
30540 if ck=3 then gosub POST_TO_JOB : goto ENTRY_SCREEN
30550 if ck=65 then goto L6520 else goto L6530
30560 L6520: ! 
30570 editrec=val(resp$(6))
30580 read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12",rec=editrec: jn$,cat,subcat,amt,jobdesc$,vn$,iv$
30590 delete #jcbreakdown,rec=editrec: noRec ENTRY_SCREEN
30600 goto ENTRY_SCREEN
30610 L6530: if ck=68 then goto L6540 else goto L6550
30620 L6540: jn$="": fnjob_srch(jn$,1) : goto ENTRY_SCREEN
30630 L6550: jn$=resp$(1)(1:6)
30640 jn$=lpad$(rtrm$(jn$),6)
30650 read #41,using 'form pos 7,c 25',key=jn$: jobname$ nokey L6590
30660 goto L6600
30670 L6590: ! 
30680 mat ml$(3)=("")
30690 ml$(1)="Job # "&jn$&" does not exist."
30700 ml$(2)="                                        "
30710 ml$(3)="Take OK to select a different job #."
30720 fnmsgbox(mat ml$,resp$,cap$,0)
30730 goto ENTRY_SCREEN
30740 L6600: if ck=69 then goto L6610 else goto L6620
30750 L6610: cn$="": fncategory_srch(cn$,1) : cat=val(cn$): goto ENTRY_SCREEN
30760 L6620: cat=val(resp$(2)(1:5))
30770 subcat=val(resp$(3)(1:3))
30780 amt=val(resp$(4))
30790 jobdesc$=resp$(5)
30800 write #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$
30810 amt=0: goto ENTRY_SCREEN
30820 POST_TO_JOB: ! 
30830 restore #jcbreakdown: 
30840 L6700: read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$ eof L6900
30850 if ltrm$(jn$)="" or rtrm$(ltrm$(jn$))="0" then goto L6700
30860 cn$=jn$&lpad$(str$(cat),5)
30870 read #2,using L6740,key=cn$: mat l,mat ta nokey L6780
30880 L6740: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
30890 l(6)=l(6)+amt
30900 l(9)=l(9)+amt
30910 goto L6780
30920 L6780: read #45,using L6790,rec=1,reserve: ot5
30930 L6790: form pos 86,pd 3
30940 ! dim empnum$*12
30950 ! empnum$=lpad$(rtrm$(str$(ji1(1))),12)
30960 L6810: ot5=lrec(45)+1
30970 invdate=val(up$(1))
30980 write #45,using L6840,rec=ot5,reserve: "",jn$,cat,subcat,0,invdate,0,0,0,0,amt,jobdesc$,0 duprec L6810
30990 L6840: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
31000 if ta(2)=0 then ta(1)=ot5 else rewrite #45,using L6790,rec=ta(2),reserve: ot5
31010 rewrite #45,using L6790,rec=1,release: ot5
31020 ta(2)=ot5
31030 rewrite #2,using L6740,key=cn$: mat l,mat ta
31040 goto L6700
31050 L6900: jn$="": jobdesc$="": amt=0: cat=subcat=0
31060 close #jcbreakdown: 
31070 execute "Drop Q:\CLmstr\jcbreakdowns"&wsid$&".h"&env$('cno')
31080 L6930: ! 
31090 return  ! /r
31500 MAKE_JCB: ! r: create Q:\CLmstr\JCBreakdownS"&wsid$&".h"&env$('cno')
31520 open #jcbreakdown=46: "Name=Q:\CLmstr\JCBreakdownS"&wsid$&".h"&env$('cno')&",Version=1,replace,RecL=79",internal,outIn,relative 
31540 close #jcbreakdown: 
31560 execute "INDEX Q:\CLmstr\JCBreakdownS"&wsid$&".h"&env$('cno')&",Q:\CLmstr\jcbrkidx"&wsid$&".H"&env$('cno')&",48,20,Replace,DupKeys -n"
31580 return  ! /r
32000 HDR2: ! r: header for jub cost listing
32020 fnopenprn
32040 print #255,using 'Form POS 1,C 8,Cc 82': date$,env$('cnam')
32060 print #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg,"Job Cost Entry Listing"
32080 print #255: ""
32100 print #255: " Payee #    Invoice #    Job #    Cat #  Sub-Cat   Amount  Descripton"
32120 print #255: " _______    _________    _____    _____  _______   ______  __________"
32140 return  ! /r
32160 ! ______________________________________________________________________
33000 PRINT_JOB_COST_ENTRIES: ! r:
33020 letotal_allocations=0
33040 gosub HDR2
33060 restore #jcbreakdown: 
33080 do 
33100   read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$ eof L7140
33120   total_allocations+=amt
33140   print #255,using "form pos 1,c 8,x 2,c 12,x 2,c 6,x 2,n 5,x 2,n 6,x 2,pic(zzz,zzz.##cr),c 30,skip 1": vn$,iv$,jn$,cat,subcat,amt,jobdesc$ pageoflow PGOF2
33160 loop 
33180 L7140: ! 
33200 print #255,using "form pos 48,c 10,skip 1,pos 48,pic(zzz,zzz.zzcr),skip 1,pos 48,c 10": "__________",total_allocations,"=========="
33220 fncloseprn
33240 jn$=jobdesc$="" : cat=subcat=amt=0
33260 return  ! /r
34000 PGOF2: ! r:
34020 print #255: newpage
34040 gosub HDR2
34060 continue  ! /r
35000 def fntest_key(holdkey$*20,vn$,iv$,cap$*128)
35020   dim newkey$*20
35040 ! uses open files:
35060   newkey$=rpad$(vn$&iv$,20)
35080   if newkey$=holdkey$ then goto TEST_KEY_OK
35100 ! 
35120 ! TEST1: !
35140 ! pass goes to test2 - fail goes to test_key_fail_on_iv
35160   close #ivpaid: ioerr ignore
35180   open #ivpaid:=fngethandle: "Name=Q:\CLmstr\IvPaid.h"&env$('cno')&",KFName=Q:\CLmstr\IvIndex.h"&env$('cno'),internal,outIn,keyed 
35200   unpaidkey$=rpad$(ltrm$(vn$),8)&rpad$(ltrm$(iv$),12)
35220   read #ivpaid,using 'Form Pos 1,C 8',key=unpaidkey$,release: x$ nokey TEST2
35240   goto TEST_KEY_FAIL_ON_IV
35260 ! ___________
35280 TEST2: ! 
35300 ! pass goes to test_key_pass - fail goes to test_key_fail_on_paytrans
35320   open #testpaytrans:=fngethandle: "Name=Q:\CLmstr\PayTrans.h"&env$('cno')&",KFName=Q:\CLmstr\UnPdIdx1.h"&env$('cno')&",SHR",internal,outIn,keyed 
35340   read #testpaytrans,using 'Form Pos 1,C 8',key=newkey$,release: x$ nokey TEST_KEY_OK
35360   goto TEST_KEY_FAIL_ON_PAYTRANS
35380 ! ____________
35400 TEST_KEY_FAIL_ON_PAYTRANS: ! 
35420   mat ml$(3)=("")
35440   ml$(1)="The invoice number "&trim$(iv$)&" for Payee "&trim$(vn$)
35460   ml$(2)="already exists in the Unpaid Invoice file."
35480   ml$(3)="Please change the Invoice Number or the Payee."
35500   fnmsgbox(mat ml$,resp$,cap$,0)
35520   goto TEST_KEY_FAIL
35540 ! ___________
35560 TEST_KEY_FAIL_ON_IV: ! 
35580   mat ml$(3)=("")
35600   ml$(1)="The invoice number "&trim$(iv$)&" for Payee "&trim$(vn$)
35620   ml$(2)="already exists in the Paid Invoice file."
35640   ml$(3)="Please change the Invoice Number or the Payee."
35660   fnmsgbox(mat ml$,resp$,cap$,0)
35680   goto TEST_KEY_FAIL
35700 ! ___________
35720 TEST_KEY_OK: ! 
35740 ! Print 'fnTest Key PASSED'
35760   fntest_key=1
35780   goto EO_TEST_KEY
35800 ! ___________
35820 TEST_KEY_FAIL: ! 
35840 ! Print 'fnTest Key FAILED'
35860   fntest_key=2
35880   goto EO_TEST_KEY
35900 ! ___________
35920 EO_TEST_KEY: ! 
35940 ! If FILE(IVPAID)<>0 Then Close #IVPAID:
35960   if file(testpaytrans)<>0 then close #testpaytrans: ioerr ignore
35980 fnend 
36000 def fn_addinvoice(vn$,iv$,airecordnumbertoedit)
36020   if ~aisetup then 
36040     aisetup=1
36060     dim aiuacolmask$(3)
36080     aiuacolmask$
36100     aiuacolmask$(1)='' ! glnMask$ <-- can't use gln mask if i want the description on it too
36120     aiuacolmask$(2)='' ! '10'
36140     aiuacolmask$(3)=''
36160     dim aiuacolhead$(3)*40
36180     aiuacolhead$(1)='General Ledger'
36200     aiuacolhead$(2)='Amount'
36220     aiuacolhead$(3)='Description'
36230     dim selected_alloc$*50
36240   end if 
36280 ! 
38000   if recordnumbertoedit then ! 
38040     editing=1
38060     read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=airecordnumbertoedit,release: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
38080     mat resp$=("")
38100     holdkey$=vn$&iv$
39000   else ! add  (ck=1)
39020     mat resp$=("")
39040     editing=0
39060     holdkey$=''
39080     vn$=''
39100     iv$=''
39120     up$(1)=''
39140     up$(2)=''
39160     up$(3) =''
39180     up$(4)=''
39200     upa=0
39220     disamt=0
39240     ddate=0
39260     pcde=0
39280     bcde=0
42100   end if 
42120 ! 
46000 AI_ADD_UNPAID_INVOICES_TOS: ! r:
46020   fnTos(sn$="ai_unpaid-2")
46040   respc=0 : mat resp$=("") : frame_width=90
46060   lc=0
46080   fnFra(1,1,11,frame_width,"Unpaid Invoice")
46100   lc=0 : mylen=18 : mypos=mylen+2
46120   frame=1
46160   fnLbl(lc+=1,1,"Payee:",mylen,right,0,frame)
46180   fncombof("Paymstr",lc,mypos,0,"Q:\CLmstr\paymstr.h"&env$('cno'),1,8,9,30,"Q:\CLmstr\Payidx1.h"&env$('cno'),1,0, "Enter the payee number (Use the 'Add Payee' option to add a new vendor record",frame)
46200   resp$(1)=vn$
46220   fnLbl(lc+=1,1,"Invoice Number:",mylen,right,0,frame)
46240   fnTxt(lc,mypos,12,0,0,"",0,"",frame)
46260   resp$(2)=iv$
46280 ! had a required answer here; temporarly changed to a message box
46300   fnLbl(lc+=1,1,"Invoice Date:",mylen,right,0,frame)
46320   fnTxt(lc,mypos,8,0,1,"mmddyy",0,"",frame)
46340   resp$(3)=up$(1)
46360   fnLbl(lc+=1,1,"Due Date:",mylen,right,0,frame)
46380   fnTxt(lc,mypos,8,0,1,"mmddyy",0,"",frame)
46400   resp$(4)=up$(2)
46420   fnLbl(lc+=1,1,"P O Number:",mylen,right,0,frame)
46440   fnTxt(lc,mypos,12,0,0,"",0,"",frame)
46460   resp$(5)=up$(3)
46480   fnLbl(lc+=1,1,"Description:",mylen,right,0,frame)
46500   fnTxt(lc,mypos,18,0,0,"",0,"",frame)
46520   resp$(6)=up$(4)(1:18)
46540   fnLbl(lc+=1,1,"Amount:",mylen,right,0,frame)
46560   fnTxt(lc,mypos,12,0,1,"10",0,"Enter the total invoice amount.",frame)
46580   resp$(7)=str$(upa)
46600   fnLbl(lc+=1,1,"Discount Amount:",mylen,right,0,frame)
46620   fnTxt(lc,mypos,12,0,1,"10",0,"Enter any discount allowed.",frame)
46640   resp$(8)=str$(disamt)
46660   fnLbl(lc+=1,1,"Discount Date:",mylen,right,0,frame)
46680   fnTxt(lc,mypos,10,0,1,"ccyymmdd",0,"",frame)
46700   resp$(9)=str$(ddate)
46720   fnLbl(lc+=1,1,"Payment Code:",mylen,right,0,frame)
46740   item1$(1)="Pay Later"
46760   item1$(2)="Pay Now"
46780   item1$(3)="Paid"
46800   fncomboa("unpaid-2",lc,mypos,mat item1$,"If you choose pay now, the invoice will be coded for payment and will paid next time checks are printed.",0,1)
46810   if pcde=0 then resp$(10)=item1$(1) ! Pay Later
46820   if pcde=1 then resp$(10)=item1$(2) ! Pay Now
46840   if pcde=2 then resp$(10)=item1$(3) ! Paid
46900   fnLbl(lc+=1,1,"Bank Code:",mylen,right,0,frame)
46920   fncombof("bankmstr",lc,mypos,23,"Q:\CLmstr\bankmstr.h"&env$('cno'),1,2,3,20,"Q:\CLmstr\bankidx1.h"&env$('cno'),0,0, "",frame)
46940   resp$(11)=str$(bcde) ! RESP$(RESPC)
46960   fnButton(1,80,"Payee",50,"Add or edit a payee",0,0,frame)
47000   fnLbl(lc=15,3,"Breakdown Information",mylen,center)
47020 ! 
47040   fnLbl(lc+=1,35,"Allocation(s):",20,right)
47060   fnButton(lc,56,"Auto by Payee [F2]",2,"Reset allocations to Payee's defaults")
47080   fnButton(lc,76,"Add",52,"Add a new allocation")
47100   fnButton(lc,81,"Edit",53,"Modify an existing allocation")
47120   fnButton(lc,87,"Delete",54,"Remove selected allocation")
48000   fnflexinit1('unpdaloc',lc+=1,2,10,88,mat aiuacolhead$, mat aiuacolmask$,1)
48020   dim alloc2d$(0,3)*30
48040   dim alloc2d_setup$*20
48060   if alloc2d_setup$<>lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12) then 
48080     alloc2d_setup$=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12)
48100     aiinvoiceallocationcount=fn_readalloctaitonsinto2darray(alloc2d_setup$,mat alloc2d$)
48120     mat alloc2d$(aiinvoiceallocationcount,udim(mat alloc2d$,2))
48140   else 
48160     aiinvoiceallocationcount=udim(mat alloc2d$,1)
48180   end if 
48200   for aiallocitem=1 to aiinvoiceallocationcount
48220     dim tmpitem$(3)*50
48240     tmpitem$(1)=fnrgl$(alloc2d$(aiallocitem,1))
48260     tmpitem$(2)=alloc2d$(aiallocitem,2)
48280     tmpitem$(3)=alloc2d$(aiallocitem,3)
48300     fnflexadd1(mat tmpitem$)
48320   next aiallocitem
49000   fnCmdKey("Save",1,1)
49020 ! fnCmdKey("&Allocate",2,0,0,"Automatically allocates the general ledger breakdown if payee record contains the breakdown information")
49040   fnCmdKey("&Delete",3,0,0,"Delete the invoice highlighted above")
49060   fnCmdKey("&Cancel",5,0,1,"Return to Unpaid Invoice selection (without saving)")
49080   fnAcs(sn$,0,mat resp$,ck)
52000   if ck=5 then 
52020     alloc2d_setup$=''
52040     mat alloc2d$=('')
52060     goto AIFINIS
52080   end if 
52100   vn$=lpad$(rtrm$(resp$(1)(1:8)),8) ! payee number
52120   iv$=lpad$(rtrm$(resp$(2)),12) ! invoice number
52130   alloc2d_setup$=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12)
52140   up$(1)=resp$(3) ! invoice date
52160   up$(2)=resp$(4) ! due date
52180   up$(3)=resp$(5) ! po number
52200   up$(4)=resp$(6)(1:18) ! description
52220   upa=val(resp$(7)) ! amount
52240   disamt=val(resp$(8)) ! discount amount
52260   ddate=val(resp$(9)) ! discount date
52280   if resp$(10)=item1$(1) then pcde=0 ! pay later
52300   if resp$(10)=item1$(2) then pcde=1 ! pay now
52320   if resp$(10)=item1$(3) then pcde=2 ! paid
52340   bcde=val(resp$(11)(1:3))
52360   selected_alloc$=fnagl$(resp$(12))
52380   if ck=3 then ! delete invoice and breakdowns
52400     fn_invoicedelete(holdkey$)
52420     goto AIFINIS
52440   else if ck=2 then 
52460     fn_invoiceallocatefrompayee(mat alloc2d$,vn$,upa,paymstr1,payeegl)
52480   else if ck=50 then 
52500 !   vn$=ss$=ph$=contact$=email$=fax$=myact$=""
52520     fnaddpayee
52540   else if ck=52 then ! Add
52560     fn_invoiceallocationfm(vn$,iv$)
52580   else if ck=53 then ! Edit
52600     fn_invoiceallocationfm(vn$,iv$, selected_alloc$)
52620   else if ck=54 then 
52640     fn_invoiceallocationdelete(selected_alloc$)
52660   else if ck=1 then ! Save
52680     if fn_invoicevalid then 
52700       fn_invoicesave
52720       if havejc=1 then gosub JOBCOST
52740       if editing=1 then 
52760         editing=0
52780         goto AIFINIS
52800       end if 
52820       goto AIFINIS ! jb 11/30/2016   !   goto ai_ADD_UNPAID_INVOICES ! setup up new invoice  kj 11609
52840     else 
52860       goto AI_ADD_UNPAID_INVOICES_TOS ! must have an invoice number
52880     end if 
52900   end if 
52920   goto AI_ADD_UNPAID_INVOICES_TOS
52940 ! /r
56000 ! 
56020 ! 
56040 AIFINIS: ! 
56060 fnend 
68000 def fn_invoicesave ! write any new invoices and matching allocations
68020   if editing=0 then 
68040     write #paytrans,using 'Form POS 1,Cr 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
68060   else if editing=1 then 
68080     rewrite #paytrans,using 'Form POS 1,Cr 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=rec(paytrans): lpad$(rtrm$(vn$),8),lpad$(rtrm$(iv$),12),mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
68100     do 
68120       delete #unpdaloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey A_AWI_DELOLDALLOCFINIS eof A_AWI_DELOLDALLOCFINIS ! delete any previous allocations because all we know about them is the invoice # and vendor#(can't identify which ones to update. change or add
68140     loop 
68160 A_AWI_DELOLDALLOCFINIS: ! 
68180   end if 
68200   for aiallocitem=1 to aiinvoiceallocationcount
68280     write #unpdaloc,using F_UNPDALOC: vn$,iv$,alloc2d$(aiallocitem,1),val(alloc2d$(aiallocitem,2)),alloc2d$(aiallocitem,3)
68290 F_UNPDALOC: form pos 1,cr 8,c 12,c 12,pd 5.2,c 30
68340   next aiallocitem
68380 fnend 
72000 def fn_invoicevalid ! very local
72020   ivreturn=1
72040 ! x=0
72060   tac=0
72080   for iv2ditem=1 to udim(mat alloc2d$,1)
72100     tac+=val(alloc2d$(iv2ditem,2))
72120   next iv2ditem
72140   if fntest_key(holdkey$,vn$,iv$,cap$)=2 then 
72160     ivreturn=0
72180   else if trim$(iv$)="" then ! must have an invoice number
72200     mat ml$(3)
72220     ml$(1)="You must enter an invoice number on each unpaid "
72240     ml$(2)="record.  If you must make up an invoice number,"
72260     ml$(3)="be careful to use a different number each time!"
72280     fnmsgbox(mat ml$,resp$,cap$,16)
72300     ivreturn=0
72320   else if tac<>upa then ! allocations don't match total invoice
72340     mat ml$(3)
72360     ml$(1)="The allocations of "&trim$(cnvrt$("pic($$$$,$$$.##)",tac))&" do not agree with"
72380     ml$(2)="the total invoice of "&trim$(cnvrt$("pic($$$$,$$$.##)",upa))&"."
72400     ml$(3)="You must correct the problem before you can continue!"
72420     fnmsgbox(mat ml$,resp$,cap$,16)
72440     ivreturn=0
72460   end if 
72480   fn_invoicevalid=ivreturn
72500 fnend 
74000 def fn_readalloctaitonsinto2darray(key$*20,mat alloc2d$)
74020   rai2a_return=0
74040   restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey AI_EO_UNPDALOC
74060   do 
74080     read #unpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,allocamount,gldesc$ eof AI_EO_UNPDALOC
74100     if vn$=hvn$ and iv$=hiv$ and (trim$(gl$&gldesc$)<>'' and allocamount<>0) then 
74120       mat alloc2d$(rai2a_return+=1,udim(mat alloc2d$,2))
74140       alloc2d$(rai2a_return,1)=gl$
74160       alloc2d$(rai2a_return,2)=str$(allocamount)
74180       alloc2d$(rai2a_return,3)=gldesc$
74220     end if 
74240   loop while vn$=hvn$ and iv$=hiv$
74260 AI_EO_UNPDALOC: ! 
74280   fn_readalloctaitonsinto2darray=rai2a_return
74300 fnend 
76000 def fn_invoiceallocatefrompayee(mat alloc2d$,vn$,upa,paymstr1,payeegl) ! ai_READ_STANDARD_BREAKDOWNS: !  pull standard gl breakdowns from payee file
76020   read #paymstr1,using "form pos 1,c 8,4*c 30,pd 5.2,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20",key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey AI_XIT
76040   ai_totalalloc=0
76060   restore #payeegl,key>=vn$: nokey AI_XIT
76080   iafp_count=0
76100   do 
76120     read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof AI_EO_READSTGL
76140     if vn$<>payeekey$ then goto AI_EO_READSTGL
76160     if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto AI_NEXT
76180     read #glmstr,using 'Form POS 13,C 30',key=payeegl$,release: de$ nokey AI_NEXT
76200     mat alloc2d$(iafp_count+=1,udim(mat alloc2d$,2))
76220     alloc2d$(iafp_count,1)=payeegl$
76240     alloc2d$(iafp_count,2)=str$(round(upa*percent*.01,2))
76260     ai_totalalloc+=val(alloc2d$(iafp_count,2))
76280     alloc2d$(iafp_count,3)=gldesc$
76300 AI_NEXT: ! 
76320   loop 
76340 AI_EO_READSTGL: ! 
76360   if ai_totalalloc<>upa and iafp_count>0 then alloc2d$(iafp_count,2)=str$(val(alloc2d$(iafp_count,2))+upa-ai_totalalloc)
76380 AI_XIT: ! 
76400 fnend 
78000 def fn_invoicedelete(holdkey$*20)
78020   delete #paytrans,key=holdkey$: ioerr AI_L4330
78040   do 
78060     delete #unpdaloc,key=holdkey$: nokey AI_L4330 eof AI_L4330 ! delete any  allocations
78080   loop  ! delete all allocations
78100 AI_L4330: ! 
78140 fnend 
82000 def fn_invoiceallocationdelete(selected_alloc$*50)
82020   for j=1 to udim(mat alloc2d$,1)
82040     iadmatch=0
82060     if alloc2d$(j,1)=selected_alloc$ then 
82080       iadmatch=j
82100       goto IADGOTIT
82120     end if 
82140   next j
82160 IADGOTIT: ! 
82180   if iadmatch then let fn2d_remove(mat alloc2d$,iadmatch)
82900 fnend 
84000 def fn_invoiceallocationfm(vn$,iv$; selected_alloc$*50)
84020   dim iaf_desc$*30
84040   iaf_gl$=''
84060   iaf_desc$=''
84080   iaf_amt=0
84100   iaf_edit=0
84120   if selected_alloc$<>'' then ! editing an allocation
84160     for j=1 to udim(mat alloc2d$,1)
84200       if alloc2d$(j,1)=selected_alloc$ then 
84220         iaf_edit=j
84240         goto IAFGOTIT
84260       end if 
84280     next j
84300 IAFGOTIT: ! 
84320     if iaf_edit then 
84340       iaf_gl$=alloc2d$(iaf_edit,1)
84360       iaf_amt=val(alloc2d$(iaf_edit,2))
84380       iaf_desc$=alloc2d$(iaf_edit,3)
84400     end if 
84420 ! else ! adding an allocation
84440 !   iaf_edit=0
84460   end if 
84480   fnTos(sn$='InvoiceAllocationFM')
84500   respc=0
84520   mylen=32 : mypos=mylen+2
84540   lc=0
84560   fnLbl(lc+=1,1,"Payee:",mylen,right)
84580   fnTxt(lc,mypos,20, 0,0,'',1)
84600   resp$(respc+=1)=vn$
84620   fnLbl(lc+=1,1,"Invoice Number:",mylen,right)
84640   fnTxt(lc,mypos,12,0,0,"",1)
84660   resp$(respc+=1)=iv$
84680   lc+=1
84700   fnLbl(lc+=1,1,"General Ledger:",mylen,right)
84720   fnqglbig(lc,mypos,0,2)
84740   resp$(iaf_respc_gl:=respc+=1)=fnrglbig$(iaf_gl$)
84760   fnLbl(lc+=1,1,"Amount:",mylen,right)
84780   fnTxt(lc,mypos,12,0,1,"10")
84800   resp$(iaf_respc_amt:=respc+=1)=str$(iaf_amt)
84820   fnLbl(lc+=1,1,"Description:",mylen,right)
84840   fnTxt(lc,mypos,18)
84860   resp$(iaf_respc_desc:=respc+=1)=iaf_desc$
84880   fnCmdSet(4)
84900   fnAcs(sn$,0,mat resp$,ckey)
84920   if ckey<>5 then 
84940     iaf_amt=val(resp$(iaf_respc_amt))
84960     iaf_gl$=fnagl$(resp$(iaf_respc_gl))
84980     iaf_desc$=resp$(iaf_respc_desc)
85000     if iaf_edit=0 then 
85020       iaf_edit=udim(mat alloc2d$,1)+1
85040       mat alloc2d$(iaf_edit,udim(mat alloc2d$,2))
85060     end if 
85080     alloc2d$(iaf_edit,1)=iaf_gl$
85100     alloc2d$(iaf_edit,2)=str$(iaf_amt)
85120     alloc2d$(iaf_edit,3)=iaf_desc$
85140   end if 
85160 fnend 
86000 def fn2d_remove(mat a2d$,iadwhich)
86020 ! removes an array item from the row (1st parameter) from an array
86040   dim a2dnew$(0,0)*30
86060   a2d_count_x=udim(mat a2d$,1)
86080   a2d_count_y=udim(mat a2d$,2)
86100   if iadwhich>0 and iadwhich<=a2d_count_x then 
86120     mat a2dnew$(a2d_count_x-1,a2d_count_y)
86140     for a2dx=1 to a2d_count_x
86160       if a2dx<>iadwhich then 
86180         a2dnewx+=1
86200         for a2dy=1 to a2d_count_y
86220           a2dnew$(a2dnewx,a2dy)=a2d$(a2dx,a2dy)
86240         next a2dy
86260       end if 
86280     next a2dx
86300     mat a2d$(a2d_count_x-1,a2d_count_y)
86320     mat a2d$=a2dnew$
86340   end if 
86360 fnend 
