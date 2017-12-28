00010 ! formerly S:\acsGL\GLInput
00020 ! enter GL transactions
00030 ! r: setup library, dims, constants, fntop
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncloseprn,fnopenprn,fnchain,fnprocess,fnfscode,fnstyp,fnTos,fnLbl,fnqgl,fnrgl$,fnTxt,fnCmdKey,fnAcs,fnagl$,fnmsgbox,fncomboa,fncombof,fnButton,fnflexinit1,fnflexadd1,fngethandle,fnaddglpayee,fnCmdSet,fnFra,fnOpt,fnreg_read,fncopy
00050   on error goto ERTN
00100 ! ______________________________________________________________________
00102   dim cap$*128,resp$(30)*128
00104   dim tr(7),tr$*12,td$*30,tcde(10,2),dedcode(10),empname$*30
00106   dim miscname$(10)*20
00108   dim key$*12,k(30,8),camt(5)
00110   dim d(2),cgl$(5)*12
00112   dim pgl(5,3)
00114   dim pr(19)
00116 ! dim contra(6)
00118   dim miscgl$(10)*12
00120   dim jv$(3)*8,vn$(4)*30
00122   dim typeofentry_option$(6)*18,message$*100,glitem$(4)*30
00124   dim glitem2$(7)*30,ml$(4)*100,text$*80,bankname$*40,k_list$(30)*12
00126   dim pr(19)
00128 ! ______________________________________________________________________
00130   fntop(program$,cap$="Enter Transactions")
00132   gltyp=7
00135   fnreg_read('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$,'False')
00136 ! fil$(1)="Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&env$('cno')&",Shr"
00138 ! fil$(2)="Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno')
00140 ! fil$(3)="Name="&env$('Q')&"\GLmstr\GLWK2"&wsid$&".h"&env$('cno')
00142 ! 
00144 ! fil$(5)="Name="&env$('Q')&"\GLmstr\GLPT"&wsid$&".h"&env$('cno')
00146 ! fil$(6)="Name="&env$('Q')&"\GLmstr\GLBRec.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno')&",Shr"
00148 ! 
00150   typeofentry_option$(1)="1 = Disbursements"
00152   typeofentry_option$(2)="2 = Receipts"
00154   typeofentry_option$(3)="3 = Adjustments"
00156   typeofentry_option$(4)="4 = Payroll Check"
00158   typeofentry_option$(5)="5 = Sales"
00160   typeofentry_option$(6)="6 = Purchases"
00162   dim cmask3$(6),chdr_proof_total$(6),glitem3$(6)
00164   chdr_proof_total$(1)='G/L Account'
00166   chdr_proof_total$(2)='Beg Balance'
00168   chdr_proof_total$(3)='Receipts'
00170   chdr_proof_total$(4)='Disbursements'
00172   chdr_proof_total$(5)='Adjustments'
00174   chdr_proof_total$(6)='End Balance'
00176   cmask3$(1)=''
00178   cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(5)="10"
00180   cmask3$(6)='10'
00182 ! General Ledger Breakdown Grid
00184   mat chdr$(5)
00186   chdr$(1)='Reference'
00188   chdr$(2)='Payee Number'
00190   chdr$(3)='GL Number'
00192   chdr$(4)='Percent'
00194   chdr$(5)='Description      '
00196   mat cmask$(5)
00198   cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)=''
00200   cmask$(4)='32'
00202   mat glitem$(5)
00204 ! 
00206   dim chdr2$(7)*25
00208   mat chdr2$(7)
00210   mat cmask2$(7)
00212   mat glitem2$(7)
00214   chdr2$(1)='Record '
00216   chdr2$(2)='Date'
00218   chdr2$(3)='Reference #'
00220   chdr2$(4)='Payee/Description'
00222   chdr2$(5)='G/L #  '
00224   chdr2$(6)='Amount'
00226   chdr2$(7)='Allocation Description'
00228   cmask2$(2)='3'
00230   cmask2$(3)=cmask2$(4)=''
00232   cmask2$(5)=''
00234   cmask2$(6)='10'
00236   cmask2$(7)=""
00238 ! 
00240   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input 
00242   read #1,using 'Form POS 150,2*N 1,POS 298,15*PD 4,POS 382,N 2,POS 418,10*C 20,10*N 1,POS 668,10*C 12': mat d,mat pgl,jccode,mat miscname$,mat dedcode,mat miscgl$
00244   close #1: 
00246 ! /r
00248   open #h_glmstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
00310 ! ______________________________________________________________________
00330   if gltyp<>1 then 
00340     open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&env$('cno')&",Shr",internal,outIn,keyed ioerr ignore ! 00340   open #1: fil$(1),internal,outIn,keyed ioerr ignore
00352     open #3: "Name="&env$('Q')&"\GLmstr\GLWK2"&wsid$&".h"&env$('cno'),internal,outIn,relative ioerr ignore ! PR Work File
00360   end if
00362   open #h_gl_work:=2: "Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno'),internal,outIn,relative ioerr ignore ! GL Work File
00364   F_2A: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
00365   F_2B: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
00370   pc=1
00380   adr=lrec(h_gl_work)
00390   if adr<1 then goto L460
00400 L400: ! 
00410   read #h_gl_work,using 'form pos 27,n 2',rec=adr: pc noRec CHECK_FOR_DELETED conv L460
00420   if pc=0 then post=1 ! if previous batch not posted, set post=1
00430   if pc>0 then goto L460 ! PREVIOUS TRANSACTIONS HAVE BEEN POSTED
00440   open #5: "Name="&env$('Q')&"\GLmstr\GLPT"&wsid$&".h"&env$('cno'),internal,outIn,relative ioerr L460
00450   goto SCREEN_1
00460 L460: ! 
00462   open #5: "Name="&env$('Q')&"\GLmstr\GLPT"&wsid$&".h"&env$('cno')&",SIZE=0,RecL=538,Replace",internal,outIn,relative 
00470   write #5,using F_5,rec=1: kn,mat k,td,tc,mat tcde
28000 SCREEN_1: ! r:
28020   edit=0
28040   fnTos(sn$="GLInput")
28060   mylen=20: mypos=mylen+3 : right=1 : rc=0
28080   fnFra(1,1,4,60,"Method of Entry","Choose the method of transaction entry.")
28100   if post=0 then text$="Regular Input" else text$="Correction or Addition to previous input"
28120   fnOpt(1,2,text$,0,1)
28140   resp$(respc_regularInput:=rc+=1)="True"
28160   fnOpt(2,2," Erase previous input transactions" ,0,1)
28180   resp$(respc_erasePrevious:=rc+=1)="False"
28200   fnOpt(3,2,"Input from Client Checkbook Diskette",0,1)
28220   resp$(respc_inputClientCL:=rc+=1)="False"
28240 ! fnOpt(4,2,"Input from Client A/R  Diskette",0,1) ! this option has errors and seems to be no longer used, so i removed it 12/29/2015
28260 ! resp$(4)="False"
28280   fnLbl(7,1,"Type Of Entry:",mylen,right)
28300   fncomboa("TypeOfEntry1",7,mypos,mat typeofentry_option$,"You must indicate the type of entry you will be entering.",18)
28310   if sel=0 then sel=3
28320   resp$(respc_entryType:=rc+=1)=typeofentry_option$(sel)
28340   ! resp$(4)=typeofentry_option$(max(1,sel)) !  for j=1 to 6
28360                            !    if sel=j then resp$(4)=typeofentry_option$(j)
28380                            !  next j
28400   fnLbl(8,1,"Bank Account:",mylen,right)
28420   fnqgl(8,mypos,0,2,pas)
28440   resp$(respc_bankGl:=rc+=1)=fnrgl$(bankgl$)
28460   fnLbl(9,1,"Process Ending Date:",mylen,right)
28480   fnTxt(9,mypos,8,0,right,"1001",0,"Process endings date must always be answered and will be the last day of the month or the last day of the period beding processed.",0 )
28500   resp$(respc_contraDate:=rc+=1)=str$(contra_entry_date)
28520   fnCmdKey("&Next",1,1,0,"Allows you to enter transactions.")
28540   fnCmdKey("&Proof Totals",3,0,0,"Provides proof totals and option to post the entries.")
28560   fnCmdKey("&Cancel",5,0,1,"Exits.")
28580   fnAcs(sn$,0,mat resp$,ckey)
28600   if ckey=5 then goto XIT
28620 ! pOST=1 ! code as unposted once leave this screen
28640   if post=1 and resp$(respc_regularInput)="True" then gosub CHECK_FOR_CONTRAS ! remove any contra entries created in previous run
28660   if resp$(respc_regularInput) ="True" then pt1=j=1
28680   if resp$(respc_erasePrevious)="True" then pt1=2
28700   if resp$(respc_inputClientCL)="True" then pt1=3
28720 ! if resp$(4)="True" then pt1=4
28740   sel=sel1=srch(mat typeofentry_option$,resp$(respc_entryType)) ! val(resp$(respc_entryType)(1:1)) ! type of transaction
28750 ! if sel<=1 then pause
28760 ! If SEL=4 Then Goto SCREEN_1 ! temporary line do not access payroll checks
28780   typeofentry$=resp$(4)
28800   key$=bankgl$=fnagl$(resp$(respc_bankGl)) ! GL number
28820   contra_entry_date=val(resp$(respc_contraDate)) ! contra entry date
28840   if ckey<>18 then  ! check bank account if editing transactions
28860     if (sel=1 or sel=2) and val(bankgl$)=0 then goto BANK_GL_FAIL ! must have GL bank account on receipts or disbursements
28880     if (sel=1 or sel=2 or sel=4) then read #h_glmstr,using "form pos 13,c 40",key=bankgl$,release: bankname$ nokey BANK_GL_FAIL
28900   end if
28920   if pt1=1 and post=0 then goto ERASE_PREVIOUS_INPUT ! regular input and previous batch posted
28940   post=1
28960   if pt1=3 or pt1=4 then gosub SCREEN_INSERT_DISKETTE
28980   if pt1=4 then goto COPY_AR_IN ! Input from client AR
29000   if pt1=2 then goto ERASE_PREVIOUS_INPUT ! erase any previous entries
29020   goto L1140 ! /r
29040 BANK_GL_FAIL: ! r:
29060   mat ml$(2)
29080   ml$(1)="You must have a Bank Account General Ledger"
29100   ml$(2)="Number for disbursements or receipts."
29120   fnmsgbox(mat ml$,resp$,cap$,49)
29140   goto SCREEN_1 ! /r
29160 ERASE_PREVIOUS_INPUT: ! r:
29180   post=1
29200   close #3: ioerr ignore
29220   close #h_gl_work: ioerr ignore
29240   close #5: ioerr ignore
29260   close #glallocations: ioerr ignore
29280   close #paymstr: ioerr ignore
29300   close #payeegl: ioerr ignore
29320   if pt1=3 then 
29340     fnCopy(dv$&"GLWK101.H"&env$('cno'),env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno'))
29360     fnCopy(dv$&"GLWK201.H"&env$('cno'),env$('Q')&"\GLmstr\GLWK2"&wsid$&".h"&env$('cno'))
29380     open #3: "Name="&env$('Q')&"\GLmstr\GLWK2"&wsid$&".h"&env$('cno'),internal,outIn,relative 
29400     open #h_gl_work:=2: "Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno'),internal,outIn,relative 
29420     open #9: "Name="&dv$&"PAYMSTR.H"&env$('cno')&",KFName="&dv$&"PAYIDX1.H"&env$('cno'),internal,input,keyed ioerr ignore
29440   else 
29460     open #3: "Name="&env$('Q')&"\GLmstr\GLWK2"&wsid$&".h"&env$('cno')&",size=0,RecL=110,Replace",internal,outIn,relative 
29480     open #h_gl_work:=2: "Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno')&",SIZE=0,RecL=104,Replace",internal,outIn,relative 
29500   end if 
29520   open #5: "Name="&env$('Q')&"\GLmstr\GLPT"&wsid$&".h"&env$('cno')&",Size=0,RecL=538,Replace",internal,outIn,relative 
29540   write #5,using F_5,rec=1: kn,mat k,td,tc,mat tcde
29560 F_5: form pos 1,pd 6.2,80*pd 6.2,pd 6.2,pd 6.2,20*n 2
29580   goto L1140 ! /r
29600 L1140: ! 
29620   close #6: ioerr ignore
29640   open #6: "Name="&env$('Q')&"\GLmstr\GLBRec.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno')&",Shr",internal,outIn,keyed 
29660   open #paymstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayMstr.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\GLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outIn,keyed 
29680   open #payeegl:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayeeGLBreakdown.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\GLmstr\Payeeglbkdidx.h"&env$('cno')&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed 
29700   close #glallocations: ioerr ignore
29720   open #glallocations=12: "Name="&env$('Q')&"\GLmstr\GLALLOCATIONS"&wsid$&env$('cno')&",Version=1,replace,RecL=59",internal,outIn,relative 
29740   if jccode then 
29760     open #8: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno')&",Shr",internal,input,keyed ioerr ignore
29780   end if 
29800   if ckey=3 then goto PROOF_TOTALS
29820 goto MAIN
34000 PREPARE_EDIT_SCREEN: ! r:
34020   transactionamt=0
34040   editmode=1
34060   gl$=""
34080   totalalloc=0
34100   read #h_gl_work,using fGlWork,rec=rn: mat tr,tr$,td$,vn$,mat jv$,key$ noRec PES_XIT ! get basic information from record clicked to find the complete transaction
34120   holdtr$=tr$
34140   close #glallocations: ioerr ignore
34160   open #glallocations=12: "Name="&env$('Q')&"\GLmstr\GLALLOCATIONS"&wsid$&env$('cno')&",Version=1,replace,RecL=59",internal,outIn,relative 
34180   restore #h_gl_work: 
34200 GLALLOCATIONS_READ: ! 
34220   read #h_gl_work,using F_2B: payeegl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof GLALLOCATIONS_EOF
34240   if trim$(tr$)<>trim$(holdtr$) then goto GLALLOCATIONS_READ
34260   if tr(6)=2 or tr(6)=7 then tr(5)=-tr(5) ! reverse signs on receipts and sales BEFORE DISPLAYING ON CORRECTION SCREEN
34280   transactionamt+=tr(5)
34300   vn$=vn$
34320   sel=tr(6)
34340   if tr(6)=1 then sel=1 ! reg disb
34360   if tr(6)=2 then sel=2 ! reg receipt
34380   if tr(6)=3 then sel=3 ! reg adj
34400   if tr(6)=4 then sel=4
34420   if tr(6)=7 then sel=5 ! 
34440   if tr(6)=8 then sel=6 ! 
34460   write #glallocations,using "form pos 1,c 12,pd 10.2,c 30,pd 5": payeegl$,tr(5),td$,rec(2)
34480   totalalloc+=tr(5) ! re-add total allocations
34500   goto GLALLOCATIONS_READ
34520 GLALLOCATIONS_EOF: ! 
34540   read #h_gl_work,using fGlWork,rec=rn: mat tr,tr$,td$,vn$,mat jv$,key$ noRec PES_XIT ! get basic information from record clicked to find the complete transaction
34560 PES_XIT: return  ! /r
36000 POSTING_OPTIONS: ! r:
36020   fnTos(sn$="GLInput6")
36040 ! fnFra(1,1,6,60,"Posting Options","You would only use the After_The_Fact options if you are maintaining the payroll records within the general ledger system.",0)
36060   fnOpt(1,2,"Post to General Ledger",0,0)
36080   resp$(1)="True"
36100   fnOpt(2,2,"Automatic Processing",0,0)
36120   resp$(2)="False"
36140   fnOpt(3,2,"Post After-The-Fact Payroll only",0,0)
36160   resp$(3)="False"
36180   fnOpt(4,2,"Post both General Ledger and After-The-Fact Payroll",0,0)
36200   resp$(4)="False"
36220   fnOpt(5,2,"Return to Menu without posting",0,0)
36240   resp$(5)="False"
36260   fnCmdSet(2)
36280   fnAcs(sn$,0,mat resp$,ckey)
36300   fnfscode(0)
36320   gosub CREATE_CONTRA_ENTRIES
36340   if resp$(5)="True" or ckey=5 then goto XIT ! return w/o posting
36360   if resp$(2)="True" then let fnprocess(1) else let fnprocess(0)
36380   if resp$(4)="True" then let fnprocess(4) ! post both
36400   if resp$(1)="True" then goto ACGLMRGE
36420   if resp$(3)="True" then let fnchain("S:\acsGL\PRMerge")
36440   open #h_process:=30: "Name="&env$('Q')&"\GLmstr\Process.h"&env$('cno')&",RecL=128,Use",internal,outIn,relative 
36460   if lrec(h_process)=0 then write #h_process,using "form pos 1,n 1": 0
36480   if resp$(2)="True" then rewrite #h_process,using "form pos 1,n 1",rec=1: 1 else rewrite #h_process,using "form pos 1,n 1",rec=1: 0 ! code for post payroll and gl both
36500   if resp$(4)="True" then rewrite #h_process,using "form pos 1,n 1",rec=1: 4 ! code for posting pr and gl both
36520   close #h_process: 
36540   if resp$(4)="True" then goto ACGLMRGE ! post both
36560   if resp$(2)="True" then let fnchain("S:\acsGL\autoproc")
36580   goto XIT ! /r
36600 XIT: fnxit
36620 ! ______________________________________________________________________
38000 RECORD_TRANSACTIONS: ! r:
38020   if tr(6)=2 or tr(6)=7 then tr(5)=-tr(5) ! reverse signs on receipts and sales
38040   if transadr>0 then 
38060     rewrite #h_gl_work,using fGlWork,rec=transadr: mat tr,tr$,td$ noRec L3280 
38080     goto L3300
38100   end if
38120   lr2=lrec(h_gl_work)+1
38140   L3280: ! 
38160   write #h_gl_work,using fGlWork,rec=lr2: mat tr,tr$,td$,vn$,mat jv$,key$
38180   fGlWork: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
38200   L3300: !
38220 return  ! /r
42000 MAIN: ! 
42020   if sel=4 and edit=0 then goto PAYROLL
42040   fnTos(sn$="GLInput2-"&str$(sel))
42060   mylen=18 : mypos=mylen+3 : right=1
42080   fnLbl(3,1,"Date:",mylen,right)
42100   fnTxt(3,mypos,8,0,right,"1",0,"Transaction date must always be answered.",0 )
42120   resp$(1)=str$(tr(4))
42140   if sel=3 then 
42160     fnLbl(4,1,"Amount:",mylen,right)
42180   else 
42200     fnLbl(4,1,"Net Amount:",mylen,right)
42220   end if 
42240   fnLbl(4,36,message$,50,left)
42260 ! 
42280   fnTxt(4,mypos,13,0,right,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
42300   if sel=3 then resp$(2)="" else resp$(2)=str$(transactionamt)
42320   fnLbl(5,1,"Reference #:",mylen,right)
42340   fnTxt(5,mypos,12,0,0,"",0,"Enter check number, receipt # or adjusting entry number",0)
42360   resp$(3)=tr$
42380   if sel=2 or sel=3 or sel=5 then 
42400     fnLbl(6,1,"Description:",mylen,right) ! for receipts
42420     fnTxt(6,mypos,30,0,left,"",0,"Brief description of transaction.",0 )
42440     resp$(4)=td$
42460   else 
42480     fnLbl(6,1,"Payee #:",mylen,right)
42500     if disable_payee=1 then 
42520       fnTxt(6,mypos,8,0,right,"",1,"Payee field disabled. Click 'Enable Payee' again to enable.",0 )
42540       resp$(4)=""
42560     else 
42580       fncombof("Paymstrcomb",6,mypos,35,env$('Q')&"\GLmstr\paymstr.h"&env$('cno'),1,8,9,39,env$('Q')&"\GLmstr\payidx1.h"&env$('cno'),0,pas, "If the payee # is known, the general ledger information can be extracted from that record.",0)
42600       resp$(4)=vn$
42620     end if 
42640   end if 
42660   fnLbl(7,1,"General Ledger #:",mylen,right)
42680   fnqgl(7,mypos,0,2,pas)
42700   resp$(5)=fnrgl$(gl$)
42720   if sel=3 then 
42740     fnLbl(7,60,"Net Adj:",8,right)
42760     fnTxt(7,70,13,0,right,"10",1,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
42780     resp$(6)=str$(totalalloc)
42800   else 
42820     fnLbl(7,60,"Amount:",8,right)
42840     fnTxt(7,70,13,0,right,"10",0,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
42860     resp$(6)=""
42880   end if 
42900   seltype=tr(6) : if tr(6)>4 then seltype=tr(6)-2
42920   if edit=1 then typeofentry_selected$=typeofentry_option$(seltype) else typeofentry_selected$=typeofentry$(4:20)
42940   fnLbl(1,4,"Type of Entry: "&typeofentry_selected$,36,right)
42960   fnLbl(1,38,"Bank Account: "&bankname$,50,right)
42980   fnLbl(9,53,"Transaction Breakdown")
43000 ! r: General Ledger Transaction Breakdown Grid
43020   mat chdr$(4)
43040   chdr$(1)='Reference'
43060   chdr$(2)='GL Number'
43080   chdr$(3)='Amount'
43100   chdr$(4)='Description'
43120   mat cmask$(4)
43140   mat cmask$=('')
43160   cmask$(3)='10'
43180   fnflexinit1('Glalloc',10,40,6,60,mat chdr$,mat cmask$,1,0,0)
43200   restore #glallocations: 
43220   mat glitem$(4)
43240   do  ! READ_GL_ALLOCATIONS: !
43260     read #glallocations,using 'Form Pos 1,c 12,pd 10.2,c 30': allocgl$,allocamt,td$ eof EO_FLEX1
43280     glitem$(1)=str$(rec(glallocations))
43300     glitem$(2)=allocgl$
43320     glitem$(3)=str$(allocamt)
43340     glitem$(4)=td$
43360     fnflexadd1(mat glitem$)
43380   loop  !  goto READ_GL_ALLOCATIONS
43400 EO_FLEX1: ! /r
43420   if sel=1 or sel=6 then 
43440     fnButton(6,61,"E&xtract",15,"Extracts general ledger numbers from payee records",1,8)
43460     if disable_payee=1 then payee_button$ ="Enable &Payee" else payee_button$ ="Disable &Payee"
43480     fnButton(6,72,payee_button$,16,"Allows you to disable or enable the payee field.",1,12)
43500     fnButton(6,87,"&Add Payee",17,"Allows you to add a payee record.",1,10)
43520   end if 
43540   fnButton(9,85,"&Edit",18,"Correct an allocation.",1,5)
43560   fnButton(9,92,"Edit Al&l",19,"Edit all allocations without returning to this screen.",1,8)
43580 ! fnLbl(17,73,"",1,right)
43600 ! fnLbl(16,1," ")
43620 ! If EDIT=1 Then Let fnCmdKey("C&hange Acct #",9,0,0,"")
43640   if editmode=1 then 
43660     fnCmdKey("&Complete",30,1,0,"Completed making corrections to this transaction.")
43680   else 
43700     fnCmdKey("&Next Transaction",1,1,0,"You are completed with this transaction and ready to move to the next transaction.")
43720   end if 
43740   fnCmdKey("&More Breakdowns",2,0,0,"More breakdowns to the same transaction.")
43760   fnCmdKey("&Review Transactions",3,0,0,"Prints a list of all transactions entered during the setting and also provides for edit options.")
43780   fnCmdKey("&Delete",7,0,0,"Deletes the entire transaction as shown on screen.")
43800   fnCmdKey("&Back",6,0,0,"Return to first screen to change transaction types or bank accounts.")
43820   if ~edit then let fnCmdKey("&Finish",9,0,1,"")
43840   fnAcs(sn$,0,mat resp$,ckey)
43860   allocamt=0
43880 ! pAS=1 ! kj 61107
43900   message$=""
43920   if extract=1 and ckey<>1 then extract=0
43940   if (ckey=9 or ckey=3) and sel1=3 and val(resp$(2))<>0 then ckey=1 ! force the last entry to write   ! KJ 50707
43960   if ckey=30 and sel1=3 and val(resp$(2))<>0 then ckey=1 ! force the last entry to write   ! KJ 50707
43980   if (ckey=9 or ckey=3 or ckey=6) and lrec(glallocations)>0 and edit=0 then goto AFP_XIT ! 4430 ! unwritten record on screen
44000   if ckey=9 and val(resp$(2))=0 then 
44020     goto PROOF_TOTALS
44040   else if ckey=3 then 
44060     gosub REVIEW_TRANS : goto MAIN
44080   else if ckey=6 then 
44100     transactionamt=0
44120     un$=tr$=""
44140     goto SCREEN_1
44160   else if ckey=7 then 
44180     goto DELETE_TRANS
44200   else if ckey=16 then 
44220     if disable_payee=0 then disable_payee=1 : goto MAIN
44240     if disable_payee=1 then disable_payee=0 : goto MAIN
44260   end if 
44280 ! 
44300   tr(4)=val(resp$(1)) ! date
44320   transactionamt=val(resp$(2)) ! amount
44340   if ckey=2 and transactionamt=0 then goto MAIN
44360   tr$=resp$(3) ! ref #
44380   vn$=vn$=resp$(4)(1:8) ! payee number
44400   glkey$=lpad$(rtrm$(vn$),8)
44420   read #paymstr, using "form pos 1,c 8",key=glkey$,release: x$ nokey PAYMSTR_GLNUM_NOKEY
44440   goto L4130
44460 PAYMSTR_GLNUM_NOKEY: ! 
44480   td$=resp$(4)(1:30)
44500   goto ALLOCATE_FROM_PAYEE ! use full response as description if not a payee name
44520 L4130: ! 
44540   td$=resp$(4)(9:38) ! transaction description = vendor name when vendor entered
44560 ALLOCATE_FROM_PAYEE: ! 
44580   if ckey=15 and editmode=0 then 
44600     extract=1
44620     goto EXTRACT ! pull allocation breakdown from payee record
44640   end if 
44660   allocgl$=fnagl$(resp$(5))
44680   allocamt=val(resp$(6))
44700   if sel=3 and ckey<>30 then allocamt=transactionamt ! create an allocation amount automatically on adjustments
44720   if allocamt=0 and ckey=1 and lrec(glallocations)=0 then allocamt=transactionamt ! allows them to press enter if only allocation without haveing to key the amount a second time
44740   if extract=1 then goto AFP_XIT
44760   if ckey=18 then goto AFP_XIT ! don't require gl # on main screen when editing transaction
44780   if ckey=30 and edit=1 then goto AFP_XIT ! don't require gl # when editing and take complete
44800   if sel=3 and (allocamt=0 or ckey=30) then goto AFP_XIT ! don't require gl # on adjustments when amount=0 (actually changing from one adjustment to the next)
44820   if sel=4 and edit=1 then goto AFP_XIT ! never should have an amount on payroll check. (done from another screen and edits all made from allocation screen
44840   if ckey=17 then goto AFP_XIT
44860   x=val(allocgl$) conv AFP_BAD_GL
44880   if edit=1 then goto AFP_XIT ! KJ 080608   DON'T CHECK FOR GENERAL NUMBER ON EDIT
44900   if val(allocgl$)=0 then goto AFP_BAD_GL else goto AFP_XIT
44920 AFP_BAD_GL: ! 
44940   mat ml$(3)
44960   ml$(1)="You must have a General Ledger Number"
44980   ml$(2)="on each allocation."
45000   ml$(3)="Click OK to enter the general ledger number."
45020   fnmsgbox(mat ml$,resp$,cap$,49)
45040   goto MAIN
46000 AFP_XIT: ! 
46020   if ~edit=1 then ! DON'T CHANGE CODE IF MAKEING CORRECTIONS
46040     if sel=1 then tr(6)=1 ! reg disb
46060     if sel=2 then tr(6)=2 ! reg receipt
46080     if sel=3 then tr(6)=3 ! adj
46100     if sel=4 then tr(6)=4 ! payroll check
46120     if sel=5 then tr(6)=7 ! sales
46140     if sel=6 then tr(6)=8 ! purchases
46160   end if 
46180   if ckey=17 then 
46200     fnaddglpayee
46220     pas=0
46240     goto MAIN
46260   else if ckey=18 then 
46280     gosub EDIT_ALLOCATIONS
46300     goto MAIN
46320   else if ckey=19 then 
46340     editall=1
46360     gosub EDIT_ALLOCATIONS
46380     goto MAIN
46400   else if ckey=30 then 
46420     goto L4460
46440   else if ckey=2 and allocamt<>0 then 
46460     write #glallocations,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocamt,td$
46480     allocgl$=""
46500     totalalloc+=allocamt
46520     allocamt=0
46540     goto MAIN
46560   else if ckey=1 and allocamt<>0 then 
46580     write #glallocations,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocamt,td$
46600     allocgl$=""
46620     totalalloc+=allocamt
46640     allocamt=0
46660   end if 
46680   if ckey=1 and sel <>3 and transactionamt<>totalalloc then message$= "Allocations don't add up!" : goto MAIN
46700 ! If CKEY=1 AND SEL=3 AND TOTALALLOC <>0 Then mESSAGE$= "Allocations don't add up!": Goto MAIN
46720   if ckey=1 or ckey=2 or (ckey=9 and edit=0 and lrec(glallocations)>0) then  ! ckey 9 thing: allow last transaction to be written if take finish
46740     L4460: !
46760     restore #glallocations: 
46780     for j=1 to lrec(glallocations)
46800       read #glallocations,using "form pos 1,c 12,pd 10.2,c 30,pd 5": allocgl$,allocamt,td$,transadr eof L4520
46820       tr(1)=val(allocgl$(1:3)) : tr(2)=val(allocgl$(4:9))
46840       tr(3)=val(allocgl$(10:12)) : tr(5)=allocamt
46860       gosub RECORD_TRANSACTIONS
46880     next j
46900     L4520: ! 
46920   else 
46940     goto L4620
46960   end if
46980   if ckey=2 then goto L4620
47000 goto CLEAN_MAIN_SCREEN
48000 CLEAN_MAIN_SCREEN: ! r: clear entry screen before returning
48020   close #glallocations: ioerr ignore
48040   open #glallocations=12: "Name="&env$('Q')&"\GLmstr\GLALLOCATIONS"&wsid$&env$('cno')&",Version=1,replace,RecL=59",internal,outIn,relative 
48060   transactionamt=0
48080   if gl_retainFieldsDuringAdd$='False' then
48100     td$=""
48120   end if
48140   fn_increment_tr
48160   vn$=gl$=""
48180   totalalloc=editmode=0 ! extract=0 kj
48200   if ckey=3 or ckey=30 then 
48220     gosub REVIEW_TRANS : goto MAIN
48240   else if ckey=6 then 
48260     transactionamt=0
48280     un$=tr$=vn$=""
48300     goto SCREEN_1
48320   end if 
48340   ! 
48360   L4620: ! 
48380   if ckey=9 then goto PROOF_TOTALS
48400 goto MAIN ! /r
52000 EXTRACT: ! r:
52020   glkey$=lpad$(rtrm$(vn$),8)
52040   restore #payeegl,key>=glkey$: nokey L4780
52060 READPAYEEGL: ! 
52080   read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,td$ eof L4740
52100   if payeekey$<>glkey$ then goto L4740
52120   if percent=0 and lrec(glallocations)=0 then percent=100
52140   allocamt=round(transactionamt*(percent*.01),2)
52160   write #glallocations,using "form pos 1,c 12,pd 10.2,c 30": payeegl$,allocamt,td$: allocgl$="": totalalloc+=allocamt: allocamt=0
52180   goto READPAYEEGL
52200 L4740: ! 
52202   if totalalloc<>transactionamt then 
52204     read #glallocations,using "Form pos 13,pd 10.2",rec=lrec(glallocations): lastallocation noRec L4770
52206   else 
52208     goto L4770
52210   end if 
52220   lastallocation+=transactionamt-totalalloc
52240   rewrite #glallocations,using "Form pos 13,pd 10.2",rec=lrec(glallocations): lastallocation
52260   L4770: !
52270   allocamt=0: ! kj  eXTRACT=0
52280   L4780: ! 
52300 goto MAIN ! /r
54000 DELETE_TRANS: ! r: deletes entire transaction
54020   mat ml$(3)
54040   ml$(1)="You have chosen to delete this entire entry."
54060   ml$(2)="Click Ok to delete this entry."
54080   ml$(3)="Click Cancel to return to main entry screen."
54100   fnmsgbox(mat ml$,resp$,cap$,49)
54120   if resp$="OK" then goto L5120 else goto MAIN
54140 L5120: restore #glallocations: 
54160 L5130: read #glallocations,using "Form pos 1,c 12,pd 10.2,c 30,pd 5": gl$,allocation,td$,transadr eof L5170
54180   delete #glallocations: 
54200   delete #h_gl_work,rec=transadr: ioerr ignore
54220   goto L5130
54240 L5170: ! 
54260   transactionamt=0
54280   vn$=gl$=tr$=""
54300   goto MAIN ! /r
57000 REVIEW_TRANS: ! r:
57020   fnTos(sn$="GLInput4")
57340   fnflexinit1('GlTrans',1,1,20,90,mat chdr2$,mat cmask2$,1,0,0)
57360   restore #h_gl_work: 
57380 L5230: read #h_gl_work,using F_2B: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof L5310
57400   if trim$(tr$)<>"" and tr$<>oldtr$ then 
57420     mat glitem2$=("")
57440     glitem2$(6)=str$(net)
57460     glitem2$(7)="Net"
57480     fnflexadd1(mat glitem2$)
57500     net=0 ! add net subtotals any time reference number changes     ( AND NET<>0)was in there
57520   end if 
57560 ! gL$=CNVRT$("pic(zz#)",TR(1))&CNVRT$("pic(zzzzz#)",TR(2))&CNVRT$("pic(zz#)",TR(3))
57580   glitem2$(1)=str$(rec(2))
57600   glitem2$(2)=str$(tr(4)) ! str$(date(days(tr(4),'mmddyy'),'ccyymmdd'))
57620   glitem2$(3)=tr$
57640   glitem2$(4)=vn$
57660   glitem2$(5)=gl$
57680   glitem2$(6)=str$(tr(5))
57700   glitem2$(7)=td$
57720   fnflexadd1(mat glitem2$)
57740   net+=tr(5) ! add net check
57760   oldtr$=tr$ ! hold reference numbers
57780   goto L5230
57800 L5310: ! 
57820   mat glitem2$=("")
57840   glitem2$(6)=str$(net): glitem2$(7)="Net"
57860   fnflexadd1(mat glitem2$)
57880   net=0 ! add net subtotals at end of listing
57900   fnCmdKey("&Add",1,0,0,"Add additional transactions or allocations.")
57920   fnCmdKey("&Edit",2,1,0,"Highlight any allocation and click Edit to change any part of the entire transaction")
57940   fnCmdKey("&Print Proof List",4,0,0,"Prints a proof list of your entries..")
57960   fnCmdKey("&Back",5,0,1,"Return to main entry screen.")
57980   fnAcs(sn$,0,mat resp$,ckey)
58000   if ckey=5 then edit=0: goto CLEAN_MAIN_SCREEN
58020   rn=val(resp$(1))
58040   if ckey=2 then edit=1 else edit=0 ! set edit mode
58060   if ckey=2 and trim$(resp$(1))="" then gosub REVIEW_TRANS : goto EO_FLEX3
58080   if ckey=2 then gosub PREPARE_EDIT_SCREEN : goto EO_FLEX3
58100   if ckey=4 then let fn_pr_proof_list
58120   goto SCREEN_1 ! (on ckey=1 or anything else)
58140 ! General Ledger Breakdown Grid
58360   fnflexinit1('PayeeGl',16,1,5,70,mat chdr$,mat cmask$,1,0,0)
58480 ! r: populate grid from PayeeGL
58500   if trim$(vn$)="" then goto EO_FLEX3
58520   restore #payeegl,key>=vn$: nokey EO_FLEX3
58540   do 
58560     read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,td$ eof EO_FLEX3
58580     if vn$<>payeekey$ then goto EO_FLEX3
58600     glitem$(1)=str$(rec(payeegl))
58620     glitem$(2)=payeekey$
58640     glitem$(3)=payeegl$
58660     glitem$(4)=str$(percent)
58680     glitem$(5)=td$
58700     fnflexadd1(mat glitem$)
58720   loop 
58740   EO_FLEX3: ! /r
58760 return  ! /r
62000 COPY_AR_IN: ! r:
62020   close #h_gl_work: 
62040   execute "COPY "&dv$&"GLWK101.H"&env$('cno')&","&"Name="&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno')&" -n"
62060   open #10: "Name="&dv$&"ARTOGL.DAT",internal,input ioerr L5670
62080   read #10,using L5650: cgl$(1),camt(1),cgl$(2),camt(2),cgl$(3),camt(3),cgl$(4),camt(4),cgl$(5),camt(5)
62100 L5650: form pos 1,c 12,n 10.2,c 12,n 10.2,c 12,n 10.2,c 12,n 10.2,c 12,n 10.2
62120   close #10: 
62140 L5670: ! 
62160   fnstyp(92)
62180   goto ACGLMRGE ! /r
64000 ACGLMRGE: fnchain("S:\acsGL\acglMrge")
66000 SCREEN_INSERT_DISKETTE: ! r:
66020   close #101: ioerr ignore
66040   open #101: "SROW=10,SCOL=19,EROW=12,ECOL=62,BORDeR=DR,CaPTION=<Choose Input Drive",display,outIn 
66060   pr #101: newpage
66080   pr f "#101,2,1,Cr 40": "Insert Input Diskette in selected drive:"
66100   if dv$="" then dv$="A"
66120   rinput fields "#101,2,42,Cu 1,UT,N": dv$
66140   dv$=dv$&":" ! if dv$="A" or dv$="B" then dv$=dv$&":" else goto L5750
66160   close #101: 
66180   return  ! /r
68000 ! <Updateable Region: ERTN>
68020 ERTN: fnerror(program$,err,line,act$,"NO")
68040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
68060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
68080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
68100 ERTN_EXEC_ACT: execute act$ : goto ERTN
68120 ! /region
69000 IGNORE: continue 
70000 PROOF_TOTALS: ! r: add and display proof totals
70010 ! r: accumulate proof totals (mat k, tc (total credits), td (total debits))
70020   mat k_list$=("            "): mat k=(0) : td=tc=0
70040   restore #h_gl_work: 
70060   do 
70080     read #h_gl_work,using "Form POS 1,c 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8 ,C 6,C 5,C 3,C 12": gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof SCREEN_PROOF_TOTALS
70082 !   pr 'read an entruy from work file:'&gl$&' - '&key$ : pause
70100 ! 
70120     for j=1 to 30
70140       if k_list$(j)<>"" and key$=k_list$(j) then goto L6100 ! found matching contra account
70160       if trim$(k_list$(j))="" then k_list$(j)=key$: goto L6100 ! found a blank contra and no previous match
70180     next j
70200     goto SCREEN_PROOF_TOTALS
70220 L6100: ! 
70230 ! 
70240     if k_list$(j)=key$ then 
70250       if tr(6)=1 or tr(6)=4 or tr(6)=8 then 
70260         k(j,6)+=tr(5)
70270         td+=tr(5)
70280         tc=tc-tr(5) ! total debits entered
70290       else if tr(6)=2 or tr(6)=7 then 
70300         k(j,5)+=tr(5)
70310         tc+=tr(5)
70320         td=td-tr(5) ! total credits entered
70330       else if tr(6)=3 then 
70340         k(j,7)+=tr(5) ! net adjustments
70350         if tr(5)<0 then tc+=tr(5)
70360         if tr(5)>0 then td+=tr(5)
70370       end if 
70380     end if 
70390   loop 
70392 ! /r
70500 SCREEN_PROOF_TOTALS: ! 
70520   fnTos(sn$="proof_totals")
70540   mylen=20: mypos=mylen+3 : right=1
70560   fnLbl(1,1,"Total Debits:",mylen,right)
70580   fnTxt(1,mypos,15,0,right,"10",1,"This is total debits including adjustments",0 )
70600   resp$(1)=str$(td)
70620   fnLbl(2,1,"Total Credits:",mylen,right)
70640   fnTxt(2,mypos,15,0,right,"10",1,"This is total credits including adjustments",0 )
70660   resp$(2)=str$(tc)
70680   fnLbl(4,1,"Type of Entry:",mylen,right)
70700   fnTxt(4,mypos,15,0,0,"",1)
70720   resp$(3)=typeofentry$ ! typeofentry_selected$
70740   fnLbl(5,1,"Bank Account: "&bankname$,mylen,right)
70760   fnTxt(5,mypos,40,0,0,"",1)
70780   resp$(4)=fnrgl$(bankgl$)
70800   fnLbl(6,1,"Process Ending Date:",mylen,right)
70820   fnTxt(6,mypos,15,0,right,"1001",1,"Process Endings Date must should be the last day of the month or the last day of the period beding processed.",0 )
70840   resp$(5)=str$(contra_entry_date)
70860   for j=1 to 30
70880     read #h_glmstr,using "form pos 87,pd 6.2",key=k_list$(j),release: k(j,4) nokey L6240 ! get last balance
70900     k(j,8)=k(j,4)-k(j,5)-k(j,6)+k(j,7) ! new balance when posted
70920 L6240: ! 
70940   next j
70960 ! 
71000   fnflexinit1('Prooftotals',8,1,15,90,mat chdr_proof_total$,mat cmask3$,1,0,0)
71020   mat glitem3$=("")
71040   for j=1 to 30
71060     if trim$(k_list$(j))<>"" then ! skip blanks
71080       glitem3$(1)=k_list$(j)
71100       glitem3$(2)=str$(k(j,4))
71120       glitem3$(3)=str$(-k(j,5))
71140       glitem3$(4)=str$(k(j,6))
71160       glitem3$(5)=str$(k(j,7))
71180       glitem3$(6)=str$(k(j,8))
71200       fnflexadd1(mat glitem3$)
71220     end if 
71240   next j
71260   fnCmdKey("Print Proof Totals",10)
71280   fnCmdKey("Print Proof List",4)
71300   fnCmdKey("&Make Corrections",1,1,0,"Allows you to make corrections to any transactions before they are posted.")
71320   fnCmdKey("&Cancel Without Posting",5,0,1,"Allows you to escape without posting this batch of entries.")
71340   fnCmdKey("&Post",2,0,0,"Will post this group of entries to the general ledger files.")
71360   fnAcs(sn$,0,mat resp$,ckey)
71380   if ckey=5 then goto XIT
71400   if ckey=4 then let fn_pr_proof_list : goto SCREEN_PROOF_TOTALS
71420   if ckey=10 then let fn_pr_proof_totals : goto SCREEN_PROOF_TOTALS
71440   if ckey=1 then gosub REVIEW_TRANS : goto MAIN
71460   if ckey=2 and td<>-tc then 
71480     mat ml$(3)
71500     ml$(1)="Total Debits of "&trim$(cnvrt$("pic(-----,---,---.##)",td))&" to not equal"
71520     ml$(2)="the total Credits of "&trim$(cnvrt$("Pic(----,---,---.##",tc))
71540     ml$(3)="Click OK to continue or Cancel to go back."
71560     fnmsgbox(mat ml$,resp$,cap$,49)
71580     if resp$="Cancel" then goto SCREEN_PROOF_TOTALS
71600   end if 
71620   if ckey=2 then goto POSTING_OPTIONS
71640   goto PROOF_TOTALS ! /r
72000   def fn_pr_proof_totals
72020 F_PPT_LINE: form pos 1,c 12,5*cr 19
72040     for j=1 to 30
72060       read #h_glmstr,using "form pos 87,pd 6.2",key=k_list$(j),release: k(j,4) nokey PPT_L6240 ! get last balance
72080       k(j,8)=k(j,4)-k(j,5)-k(j,6)+k(j,7) ! new balance when posted
72100 PPT_L6240: ! 
72120     next j
72140 ! 
72160     fnopenprn
72180     pr #255: lpad$("Total Debits:",mylen)&' '&cnvrt$("pic(-------,---,---.##)",td)
72200     pr #255: lpad$("Total Credits:",mylen)&' '&cnvrt$("Pic(-------,---,---.##",tc)
72210     pr #255: ""
72220     pr #255: lpad$("Type of Entry:",mylen)&' '&typeofentry$
72240     pr #255: lpad$("Bank Account:",mylen)&' '&fnrgl$(bankgl$)
72260     pr #255: lpad$("Process Ending Date:",mylen)&' '&cnvrt$("pic(zz/zz/zz)",contra_entry_date)
72280     pr #255: ""
72300     pr #255,using F_PPT_LINE: mat chdr_proof_total$
72320     mat glitem3$=("")
72340     for j=1 to 30
72360       if trim$(k_list$(j))<>"" then ! skip blanks
72380         glitem3$(1)=k_list$(j)
72400         glitem3$(2)=cnvrt$("Pic(-------,---,---.##)",k(j,4)) ! cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(5)="10"
72420         glitem3$(3)=cnvrt$("Pic(-------,---,---.##)",-k(j,5))
72440         glitem3$(4)=cnvrt$("Pic(-------,---,---.##)",k(j,6))
72460         glitem3$(5)=cnvrt$("Pic(-------,---,---.##)",k(j,7))
72480         glitem3$(6)=cnvrt$("Pic(-------,---,---.##)",k(j,8)) ! cmask3$(6)='10'
72500         pr #255,using F_PPT_LINE: mat glitem3$
72520       end if 
72540     next j
72560     fncloseprn
72580   fnend 
74000 CREATE_CONTRA_ENTRIES: ! r:
74020   restore #h_gl_work: 
74040   tr$="999999999999"
74060   for j=1 to 30
74080     if val(k_list$(j))<>0 then 
74100       if k(j,5)<>0 then write #h_gl_work,using F_2A,rec=lrec(h_gl_work)+1: k_list$(j),contra_entry_date,-k(j,5),2,0,tr$,"Contra Entry","","","","" ! debits from receipts or sales
74120       if k(j,6)<>0 then write #h_gl_work,using F_2A,rec=lrec(h_gl_work)+1: k_list$(j),contra_entry_date,-k(j,6),1,0,tr$,"Contra Entry","","","","" ! credits from checks or purchases
74130     end if 
74140   next j
74180   return  ! /r
76000 PAYROLL: ! r:
76020   if sel=0 then sel=4 ! default to payroll
76040   fnTos(sn$="GLInput7")
76060   mylen=18: mypos=mylen+3 : right=1
76080   fnLbl(3,1,"Date:",mylen,right)
76100   fnTxt(3,mypos,8,0,right,"1",0,"Transaction date must always be answered.",0 )
76120   resp$(1)=str$(tr(4))
76140   if sel=3 then let fnLbl(4,1,"Amount:",mylen,right) else let fnLbl(4,1,"Net Amount:",mylen,right)
76160   fnLbl(4,36,message$,50,left)
76180 ! 
76200   fnTxt(4,mypos,12,0,right,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
76220   if sel=3 then resp$(2)="" else resp$(2)=str$(transactionamt)
76240   fnLbl(5,1,"Reference #:",mylen,right)
76260   fnTxt(5,mypos,12,0,0,"",0,"Enter check number.",0)
76280   resp$(3)=tr$
76300   fnLbl(6,1,"Employee #:",mylen,right)
76320   fncombof("PRmstr",6,mypos,35,env$('Q')&"\GLmstr\PRmstr.h"&env$('cno'),1,4,5,30,env$('Q')&"\GLmstr\PRINDEX.h"&env$('cno'),1,pas, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0)
76340   resp$(4)=str$(pr(1))
76360   fnLbl(7,1,"General Ledger #:",mylen,right)
76380   fnqgl(7,mypos,0,2,pas)
76400   resp$(5)=fnrgl$(gl$)
76420   if sel=3 then let fnLbl(7,60,"Net Adj:",mylen,right) else let fnLbl(7,60,"Amount:",mylen,right)
76440   if sel=3 or sel=4 then disable=1 else disable=0
76460   fnTxt(7,70,13,0,right,"10",disable,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
76480   if sel=3 then resp$(6)=str$(totalalloc) else resp$(6)=""
76500   fnLbl(1,4,"Type of Entry: "&typeofentry$(4:20),36,right)
76520   fnLbl(1,38,"Bank Account: "&bankname$,50,right)
76540   fnFra(8,1,10,70,"Payroll Breakdown","Enter the check breakdown.")
76560   fnLbl(1,1,"Total Wage:",mylen,right,0,1)
76580   fnTxt(1,22,12,0,right,"10",0,"Total wage before any deductions (gross).",1)
76600   resp$(7)=str$(pr(2))
76620   fnLbl(2,1,"Federal W/H:",mylen,right,0,1)
76640   fnTxt(2,22,12,0,right,"10",0,"Total Federal withholdings entered as a positive figure).",1)
76660   resp$(8)=str$(pr(3))
76680   fnLbl(3,1,"Fica W/H:",mylen,right,0,1)
76700   fnTxt(3,22,12,0,right,"10",0,"Total Fica withholdings entered as a positive figure).",1)
76720   resp$(9)=str$(pr(4))
76740   fnLbl(4,1,"State W/H:",mylen,right,0,1)
76760   fnTxt(4,22,12,0,right,"10",0,"Total state withholdings entered as a positive figure).",1)
76780   resp$(10)=str$(pr(5))
76800   fnLbl(5,1,"Local W/H:",mylen,right,0,1)
76820   fnTxt(5,22,12,0,right,"10",0,"Total local withholdings entered as a positive figure).",1)
76840   resp$(11)=str$(pr(6))
76860   for j=1 to 5
76880     fnLbl(j+5,1,trim$(miscname$(j))&":",mylen,right,0,1)
76900     fnTxt(j+5,22,12,0,right,"10",0,"Total "&trim$(miscname$(j))&" (enter as a positive figure).",1)
76920     resp$(j+11)=str$(pr(j+6))
76940   next j
76960   for j=6 to 10
76980     fnLbl(j-5,30,trim$(miscname$(j))&":",mylen,right,0,1)
77000     fnTxt(j-5,51,12,0,right,"10",0,"Total "&trim$(miscname$(j))&" (enter as a positive figure).",1)
77020     resp$(j+11)=str$(pr(j+6))
77040   next j
77060   fnLbl(6,30,"Tips:",mylen,right,0,1)
77080   fnTxt(6,51,12,0,right,"10",0,"Total tips entered as a positive figure).",1)
77100   resp$(22)=str$(pr(17))
77120   fnLbl(7,30,"Weeks Worked:",mylen,right,0,1)
77140   fnTxt(7,51,12,0,right,"30",0,"Total weeks worked during pay period.",1)
77160   resp$(23)=str$(pr(18))
77180   fnLbl(8,30,"Eic:",mylen,right,0,1)
77200   fnTxt(8,51,12,0,right,"10",0,"Total Earned Income Credit applied.",1)
77220   resp$(24)=str$(pr(19))
77240   if sel=1 or sel=6 then let fnButton(6,53,"E&xtract",15,"Extracts general ledger numbers from payee records",1,8)
77260   if disable_payee=1 and (sel=1 or sel=6) then payee_button$ ="Enable &Payee" else payee_button$ ="Disable &Payee"
77280   if sel=1 or sel=6 then let fnButton(6,64,payee_button$,16,"Allows you to disable or enable the payee field.",1,12)
77300   if sel=1 or sel=6 then let fnButton(6,79,"&Add Payee",17,"Allows you to add a payee record.",1,10)
77320 ! fnLbl(17,73,"",1,right)
77340 ! fnLbl(16,1," ")
77360 ! If EDIT=1 Then Let fnCmdKey("C&hange Acct #",9,0,0,"")
77380   if editmode=1 then 
77400     fnCmdKey("&Complete",30,1,0,"Completed making corrections to this transaction.")
77420   else 
77440     fnCmdKey("&Next Transaction",1,1,0,"You are completed with this transaction and ready to move to the next transaction.")
77460   end if 
77480   fnCmdKey("&Review Transactions",3,0,0,"Prints a list of all transactions entered during the setting and also provides for edit options.")
77500   fnCmdKey("&Delete",7,0,0,"Deletes the entire transaction as shown on screen.")
77520   fnCmdKey("&Back",6,0,0,"Allows you to return to screen 1 and change transaction types or bank accounts.")
77540   fnCmdKey("&Finish",9,0,1,"")
77560   fnAcs(sn$,0,mat resp$,ckey)
77580   if ckey=9 then 
77600     goto SCREEN_1
77620   else if ckey=6 then 
77640     transactionamt=0
77660     mat pr=(0)
77680     tr$=""
77700     vn$=""
77720     goto SCREEN_1
77740   else if ckey=3 then 
77760     gosub REVIEW_TRANS : goto MAIN
77780   end if 
77800   tr(4)=val(resp$(1)) ! date
77820   tr(5)=transactionamt=val(resp$(2)) ! amount
77840   tr$=resp$(3) ! ref #
77860   pr(1)=val(resp$(4)(1:4)): vn$=resp$(4)(1:4) ! employee number
77880   td$=resp$(4)(5:30) ! transaction description = employee name
77900   empname$=td$
77920   gl$=fnagl$(resp$(5))
77940   for j=2 to 19
77960     pr(j)=val(resp$(j+5))
77980   next j
78000   wh=0
78020   for j=1 to 19
78040     if j>2 and j<7 then wh=wh+pr(j)
78060     if j<7 or j>16 then goto L7300
78080     if dedcode(j-6)=2 then wh=wh-pr(j) else wh=wh+pr(j)
78100 L7300: if j=17 then wh=wh+pr(j)
78120     if j=19 then wh=wh-pr(j)
78140   next j
78160   if tr(5)=pr(2)-wh then goto WRITE_PAYROLL_TRANS
78180   mat ml$(3)
78200   ml$(1)="Total wages less deductions do not equal the net check!"
78220   ml$(2)=" Net entered:" &ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",transactionamt))&"   Calculated net: "&ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",pr(2)-wh))
78240   ml$(3)="Click ok to return to the entry screen."
78260   fnmsgbox(mat ml$,resp$,cap$,49)
78280   goto PAYROLL ! /r
80000 WRITE_PAYROLL_TRANS: ! r:
80020   tr(6)=4 ! payroll transaction type  (was converted back to 1 in old system)
80040   for j=2 to 19
80060     jv$(2)=str$(j-1) ! carry breakdown code for posting employee record
80080     if j=2 then allocgl$=gl$: td$="Gross Pay-"&empname$(1:18)
80100     if j=3 then td$="Federal Withholdings"
80120     if j=4 then td$="FICA Withholdings"
80140     if j=5 then td$="State Withholdings"
80160     if j=6 then td$="Local Withholdings"
80180     if j=3 or j=4 or j=5 or j=6 then allocgl$=cnvrt$("pic(zz#)",pgl(j-2,1))&cnvrt$("pic(zzzzz#)",pgl(j-2,2))&cnvrt$("pic(zz#)",pgl(j-2,3))
80200     if j=19 then allocgl$=cnvrt$("pic(zz#)",pgl(5,1))&cnvrt$("pic(zzzzz#)",pgl(5,2))&cnvrt$("pic(zz#)",pgl(5,3)) ! eic
80220     if j>6 and j<17 then 
80240       allocgl$=miscgl$(j-6)
80260       td$=miscname$(j-6) ! miscellaneous deductions
80280     end if 
80300     if j>2 and j<7 then 
80320       pr(j)=-pr(j) ! reverse sign on fica, etc
80340     else if j>6 and j<17 and dedcode(j-6)=1 then 
80360       pr(j)=-pr(j)
80380 ! .     ! turn sign around on any of ten deductions coded as additions
80400     else if j=17 then 
80420       pr(j)=-pr(j)
80440       td$="Tips"
80460       allocgl$=gl$ ! tips
80480     else if j=19 then 
80500       pr(j)=pr(j)
80520       td$="Eic" ! eic as positive
80540     end if 
80560     if pr(j)<>0 then 
80580       write #h_gl_work,using F_2A,rec=lrec(h_gl_work)+1: allocgl$,tr(4),pr(j),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ ! gross wage
80600     end if 
80620   next j
80640   transactionamt=0
80660   mat pr=(0)
80680   vn$=""
80700   fn_increment_tr
80820   goto MAIN ! /r
82000 CHECK_FOR_CONTRAS: ! r:
82020   for j=1 to lrec(h_gl_work)
82040     read #h_gl_work,using F_2B,rec=j: gl$,tr(4),tr(5),tr(6),tr(7),tr$ noRec CFC_NEXT_J
82060     if trim$(tr$)="999999999999" then delete #h_gl_work, rec=j: 
82080 CFC_NEXT_J: ! 
82100   next j
82120   gl$="": tr(4)=tr(5)=tr(6)=tr(7)=0: tr$=""
82140   return  ! /r
82160 CHECK_FOR_DELETED: ! r:
82180   if adr>1 then adr=adr-1: goto L400
82200   goto L460 ! /r
84000   def fn_increment_tr
84020     x=0
84040     x=val(tr$) conv L4580
84060     if tr$<>"999999999999" then 
84080       tr$=str$(val(tr$)+1) ! increment if possible
84100     else 
84120 L4580: ! 
84130       if gl_retainFieldsDuringAdd$='False' then
84140         tr$=""
84150       end if
84160     end if 
84180   fnend 
86000 EDIT_ALLOCATIONS: ! r:  editing glallocation while still being entered into allocation grid
86020   editrecord=val(resp$(7))
86040   if editall=19 then editrecord=1
86060 EA_READ_GLALLOC: ! 
86080   read #glallocations,using "Form pos 1,c 12,pd 10.2,c 30,pd 5",rec=editrecord: gl$,allocation,td$,transadr noRec EA_FINIS
86100   holdallocation=allocation
86120   fnTos(sn$="GLInput3")
86140   mylen=18: mypos=mylen+3 : right=1
86160   fnLbl(2,1,"Amount:",mylen,right)
86180   fnTxt(2,mypos,13,0,right,"10",0,"Enter the amount of this breakdown.",0 )
86200   resp$(1)=str$(allocation)
86220   fnLbl(1,1,"General Ledger #:",mylen,right)
86240   fnqgl(1,mypos,0,2,pas)
86260   resp$(2)=fnrgl$(gl$)
86280   fnLbl(3,1,"Description:",mylen,right)
86300   fnTxt(3,mypos,30,0,left,"",0,"Enter description to be carried in the general ledger transaction.",0 )
86320   resp$(3)=td$
86340   fnCmdKey("&Next",1,1,0,"Apply any changes and return to main entry screen.")
86360   fnCmdKey("&Delete",6,0,0,"Deletes this allocation.")
86380   fnCmdKey("&Cancel",5,0,1,"Return to main entry screen without applying changes.")
86400   fnAcs(sn$,0,mat resp$,ckey)
86420   if ckey=5 then goto EA_FINIS
86440   if ckey=6 then 
86450     mat ml$(3)
86460     ml$(1)="You have chosen to delete this allocation."
86480     ml$(2)="Click OK to delete this entry."
86500     ml$(3)="Click Cancel to return to previous screen."
86520     fnmsgbox(mat ml$,resp$,cap$,49)
86540     if resp$<>"OK" then 
86560       goto EDIT_ALLOCATIONS
86580     end if 
86600     if ckey=6 then delete #glallocations,rec=editrecord: 
86620     delete #h_gl_work,rec=transadr: ioerr ignore
86640     transactionamt+=-holdallocation
86660   else 
86680     allocgl$=fnagl$(resp$(2))
86700     allocation=val(resp$(1))
86720     transactionamt+=allocation-holdallocation ! update net amount of transaction
86740     td$=resp$(3)
86760     rewrite #glallocations,using "Form pos 1,c 12,pd 10.2,c 30",rec=editrecord: allocgl$,allocation,td$
86780     totalalloc+=allocation-holdallocation ! 
86800   end if 
86820   if editall=1 then 
86840     editrecord=editrecord+1
86860     goto EA_READ_GLALLOC
86880   end if 
86900 EA_FINIS: ! 
86920   editall=0
86940   return  ! /r
88000   def fn_pr_proof_list
88020     fnopenprn
88040     gosub PROOF_LIST_HDR
88060     holdtr$="" : tr$=""
88080     restore #h_gl_work: 
88100     do 
88120       holdtr$=tr$
88140       read #h_gl_work,using F_2B: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof PE_FINIS
88160       if trim$(holdtr$)<>"" and holdtr$<>tr$ then 
88180         pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount
88200         netamount=0
88220       end if 
88240       if tr(6)=3 then prntkey$="" else printkey$=key$
88260       pr #255,using F_PE_LINE: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,printkey$,mat jv$ pageoflow PROOF_LIST_PGOF
88280 F_PE_LINE: form pos 1,c 12,x 2,pic(zz/zz/zz),n 11.2,x 4,pic(zz),pic(zz),c 13,c 30,c 10,c 12,c 7,c 7,c 41
88300       netamount+=tr(5)
88320     loop 
88340 PE_FINIS: ! 
88360     pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount : netamount=0
88380     fncloseprn
88400   fnend 
90000 PROOF_LIST_PGOF: ! r:
90020   pr #255: newpage
90040   gosub PROOF_LIST_HDR
90060   continue  ! /r
92000 PROOF_LIST_HDR: ! r:
92020   pr #255: ""
92040   pr #255,using 'form pos 1,c 8,pos 29,Cc 40,skip 1,pos 1,c 8,pos 40,c 40': date$,env$('cnam'),time$,"GL Input Proof List"
92060   pr #255: ""
92080   pr #255: "   Account #    Date       Amount    TC  Reference #  Payee/Description             Vendor";
92100   if jccode=1 then pr #255: "Job #   Cat  S-Cat" else pr #255: " "
92120   return  ! /r
