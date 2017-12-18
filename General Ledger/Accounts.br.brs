00010 ! Formerly S:\acsGL\glMaint
00020 ! General Ledger Master File editor
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnerror,fntos,fnopt,fnlbl,fncmdset,fnacs,fnagl$,fnfra,fntxt,fncombof,fncmdkey,fnmsgbox,fnaccount_search,fnflexinit1,fnflexadd1,fnqglbig,fnrglbig$,fnbutton
00050 ! fnrglbig$ and fnqglbig  were added so all of the description could easily be seen in the main gl screen
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00090   dim cap$*128
00100   dim tr(7),tr$*12,td$*30
00110   dim d$*50,bc(13),bp(13),bm(13),rf(6),key$*12
00120   dim ta(2),revb(13)
00130   dim ack$*20,resp$(100)*60
00150   dim ml$(3)*128,item$(9)*30
00152   ! dim de$*50
00154   ! dim e(13),h(13),g(13),rf2(6)
00180 ! ______________________________________________________________________
00190   fntop(program$,cap$="General Ledger Master")
00220   fixgrid=99
00260   open #company=1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input 
00270   read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
00280   close #company: 
00290   open #8: "Name="&env$('Q')&"\CLmstr\GLmstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\GLIndex.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L310
00300   cl1=1
00310 L310: ! 
00320   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.H"&env$('cno')&",Shr",internal,outin,keyed 
00330   open #11: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\glIndx2.H"&env$('cno')&",Shr",internal,outin,keyed ! ioerr L350
00340 ! goto L400
00350 ! should be done in checkfileversion, not here ! L350: close #1: ioerr ignore
00360 ! should be done in checkfileversion, not here !   close #11: ioerr ignore
00370 ! should be done in checkfileversion, not here !   fnindex_it(env$('Q')&"\GLmstr\GLmstr.h"&env$('cno'),env$('Q')&"\GLmstr\glIndx2.H"&env$('cno'),"13 30")
00380 ! should be done in checkfileversion, not here !   fnindex_it(env$('Q')&"\GLmstr\GLmstr.h"&env$('cno'),env$('Q')&"\GLmstr\GLIndex.H"&env$('cno'),"1 12")
00390 ! should be done in checkfileversion, not here !   goto L310
00400 ! L400: 
00402   open #2: "Name="&env$('Q')&"\GLmstr\GLTRANS.H"&env$('cno')&",Shr",internal,outin,relative 
00410   open #3: "Name="&env$('Q')&"\GLmstr\ACTRANS.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\ACTRIDX.H"&env$('cno')&",Shr",internal,outin,keyed ioerr MAIN
00420 MAIN: ! 
00430   fntos(sn$="GLProb2-"&str$(edit_mode))
00432   mylen=23: mypos=mylen+3 : right=1
00440   fnlbl(1,1,"General Ledger Number:",mylen,right)
00450   if edit_mode=1 then ! attempt to put disabled text box for acct #
00452     fntxt(1,mypos,60,0,0,"",1,"",0)
00454     resp$(1)=fnrglbig$(gl$) ! lpad$(gl$,12)
00456 !   fnlbl(1,40,de$)
00458   else 
00460     fnqglbig(1,mypos,0,2)
00462     resp$(1)=fnrglbig$(gl$)
00464   end if 
00470   holdgl$=gl$
00472   if edit_mode=1 then 
00480     fnlbl(2,1,"Beginning Balance:",mylen,right)
00490     fntxt(2,mypos,14,0,right,"10",0,"The beginning balance will always be the account balance at the beginning of the period. It gets updated when you close the month.",0 )
00492     resp$(2)=str$(bb)
00500     fnlbl(2,40,"Current Balance:",mylen,right)
00510     fntxt(2,66,14,0,right,"10",0,"The current balance will be updated any time a transaction is posted. The beginning and current balances should both be the same when you begin.",0 )
00512     resp$(3)=str$(cb)
00522     f1Col1Len=21 
00524     f1Col2=1+f1Col1Len+2 : f1Col2Len=36
00526     f1Col3=f1Col2+f1Col2Len+2 : f1Col3len=21
00528     f1Col4=f1Col3+f1Col3len+2 : f1Col4Len=36
00529     fnfra(4,1,4,f1Col4+f1Col4Len+2,"Financial Statement Information"," ",0)
00530     fnlbl(1,1,"Balance Sheet Ref:",f1Col1Len,right,0,1)
00540     fncombof("fs-bal",1,f1Col2,f1Col2Len,env$('Q')&"\GLmstr\acglfnsb.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsbindx.h"&env$('cno'),0,0, "Select the balance sheet reference number where this account should appear on the balance sheet.",1)
00542     resp$(4)=str$(rf(1)) ! balance sheet ref #
00550     fnlbl(1,f1Col3,"2nd Balance Sheet:",f1Col3len,right,0,1)
00560     fncombof("fs-bal2",1,f1Col4,f1Col4Len,env$('Q')&"\GLmstr\acglfnsc.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnscindx.h"&env$('cno'),0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",1)
00562     resp$(5)=str$(rf(2)) ! balance sheet ref #
00570     fnlbl(2,1,"Income Statement Ref:",f1Col1len,right,0,1)
00580     fncombof("fs-inc",2,f1Col2,f1Col2Len,env$('Q')&"\GLmstr\acglfnsi.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsiindx.h"&env$('cno'),0,0, "Select the income statement reference number where this account should appear on the income statement.",1)
00582     resp$(6)=str$(rf(3)) ! income statement ref #
00590     fnlbl(2,f1Col3,"2nd Income Statement:",f1Col3len,right,0,1)
00600     fncombof("fs-inc2",2,f1Col4,f1Col4Len,env$('Q')&"\GLmstr\acglfnsj.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsjindx.h"&env$('cno'),0,0, "Select the income statement reference number where this account should appear on the secondary income statement.",1)
00602     resp$(7)=str$(rf(4)) ! 2nd income statement
00610     fnlbl(3,1,"Cash Flow/Fund Ref:",f1Col1len,right,0,1)
00620     fncombof("fs-cash",3,f1Col2,f1Col2Len,env$('Q')&"\GLmstr\acglfnsf.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsfindx.h"&env$('cno'),0,0, "Select the cash flow reference number where this account should appear on the cash flow statement.",1)
00622     resp$(8)=str$(rf(5)) ! income statement ref #
00630     fnlbl(3,f1Col3,"2nd Cash Flow/Fund:",f1Col3len,right,0,1)
00640     fncombof("fs-cash2",3,f1Col4,f1Col4Len,env$('Q')&"\GLmstr\acglfnsg.h"&env$('cno'),1,5,6,30,env$('Q')&"\GLmstr\Fnsgindx.h"&env$('cno'),0,0, "Select the cash flow reference number where this account should appear on the cash flow statement.",1)
00642     resp$(9)=str$(rf(6)) ! 2nd cash flow

00650     fnlbl(10,1,"EOY Balance - 2 Years Ago:",30,right,0,0)
00660     fntxt(10,33,12,0,right,"10",0,"In order to pr prior year's cash flow and fund statements, the final balance from two years ago must be retained.",0)
00662     resp$(10)=str$(pbp)
00670     fnfra(12,1,14,105,"History and Budget Information"," ",0)
00680     x=11
00690     fnlbl(1,14,"Balance This Yr",15,0,0,2)
00691     fnlbl(1,34,"Balance Last Yr",15,0,0,2)
00692     fnlbl(1,54,"Original Budget",15,0,0,2)
00693     fnlbl(1,74,"Revised Budget ",15,0,0,2)
00700     for j=1 to 13
00710       fnlbl(j+1,1,"Period "&str$(j),90,0,0,2)
00712       fntxt(j+1,14,14,0,right,"10",0,"",2 )
00714       resp$(x)=str$(bc(j))
00716       x=x+1
00718       fntxt(j+1,34,14,0,right,"10",0,"",2 )
00720       resp$(x)=str$(bp(j))
00722       x=x+1
00724       fntxt(j+1,54,14,0,right,"10",0,"",2 )
00726       resp$(x)=str$(bm(j))
00728       x=x+1
00730       fntxt(j+1,74,14,0,right,"10",0,"",2 )
00732       resp$(x)=str$(revb(j))
00734       x=x+1
00736     next j
00740   end if 
00770   if edit_mode=1 then 
00772     fncmdkey("&Save",6,1,0,"")
00774     fncmdkey("Review &Transactions",3,0,0,"")
00776     fncmdkey("&Delete",7,0,0,"")
00778     fnbutton(1,mypos+60+2,"C&hange",9,"Change the Account Number and/or the Description of this General Ledger Account") ! fncmdkey("C&hange Acct-Desc",9,0,0,"")
00780   else 
00782     fncmdkey("&Edit",1,1,0,"") ! if edit_mode=1 then let fncmdkey("&Edit",1,0,0,"") else let fncmdkey("&Edit",1,1,0,"")
00784     fncmdkey("&Add",2,0,0,"")
00786     fncmdkey("Sea&rch",8,0,0,"")
00788   end if 
00790   fncmdkey("&Cancel",5,0,1,"")
00850   fnacs(sn$,0,mat resp$,ckey)
00870   if ckey=5 then goto XIT
00880 ! If edit_mode=1 Then kEY$=GL$=LPAD$(RESP$(1)(1:12),12): Goto 885
00890   gl$=fnagl$(resp$(1)) : key$=gl$
00900   if ckey=7 then gosub DELETE_ACCT : goto L1780
00910   if edit_mode=1 and gl$<>holdgl$ then goto MSGBOX4 else goto L960
00960 L960: ! 
00970 ! de$=resp$(1)(13:60)
00972   if edit_mode=1 then 
00980     bb=val(resp$(2)) ! beginning bal
00990     cb=val(resp$(3)) ! current balance
01000     for j=1 to 6
01010       rf(j)=val(resp$(j+3)(1:5)) ! incomestatement reference numbers
01020     next j
01030     pbp=val(resp$(10)) ! prior years 2 years ago
01040     x=11
01050     for j=1 to 13
01060       bc(j)=val(resp$(x)): x+=1
01070       bp(j)=val(resp$(x)): x+=1
01080       bm(j)=val(resp$(x)): x+=1
01090       revb(j)=val(resp$(x)): x+=1
01100     next j
01102   end if 
01104   if ckey<>3 then edit_mode=0
01110   if ckey=1 then goto DO_EDIT
01120   if ckey=2 then goto ADD ! add new accounts
01130   if ckey=3 then goto REVIEW_TRANS ! review current or prior transactions
01140   if ckey=6 then goto SAVE
01150   if ckey=8 then goto SEARCH_GRID
01160   if ckey=9 then goto CHANGE_ACCT_NUM
01170   goto MAIN
01180 ! ______________________________________________________________________
01200 ! BLDRANGE: fnchain("S:\acsGL\BldRange")
01210 XIT: fnxit
01220 ! ______________________________________________________________________
01230 ADD: ! 
01240   fntos(sn$="GLAdd")
01242   mylen=23: mypos=mylen+3 : right=1: rc=0
01250   if use_dept =1 then let fnlbl(1,26,"Fund #",6,2)
01260   if use_sub =1 then let fnlbl(1,40,"Sub #",6,2)
01270   fnlbl(2,1,"General Ledger Number:",mylen,right)
01280   if use_dept=1 then let fntxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 )
01282   resp$(rc+=1)=str$(dno)
01290   fntxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 )
01292   resp$(rc+=1)=str$(ano)
01300   if use_sub=1 then let fntxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 )
01302   resp$(rc+=1)=str$(sno)
01310   fnlbl(3,1,"Description:",mylen,right)
01320   fntxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
01322   resp$(rc+=1)=""
01330   fncmdset(2)
01340   fnacs(sn$,0,mat resp$,ckey)
01360   if ckey=5 then goto MAIN
01370   fixgrid=99
01380   dno=ano=sno=0
01390   if use_dept=1 then dno=val(resp$(1)) : ano=val(resp$(2))
01400   if use_dept=0 then ano=val(resp$(1))
01410   if use_dept=1 and use_sub=1 then sno=val(resp$(3))
01420   if use_dept=0 and use_sub=1 then sno=val(resp$(2))
01430   if use_dept=1 and use_sub=1 then d$=resp$(4)
01440   if use_dept=0 and use_sub=1 then d$=resp$(3)
01450   if use_dept=0 and use_sub=0 then d$=resp$(2)
01460   if use_dept=1 and use_sub=0 then d$=resp$(3)
01470   key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
01480   read #1,using 'Form POS 1,N 3',key=key$: dno nokey ignore
01530 ! NEW_RECORD: !
01540 ! L1540: ! 
01541   bb=cb=pbp=0
01542   mat bc=(0): mat bp=(0): mat bm=(0): mat revb=(0): mat ta=(0): mat rf=(0)
01543   edit_mode=1
01544   gl$=key$
01550 ! write_new_record
01560   write #1,using L1740: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
01562   if cl1=1 then 
01564     write #8,using L1740: gl$,d$
01566   end if 
01568   goto MAIN
01570 ! ______________________________________________________________________
01580 DO_EDIT: ! r:
01582   pr newpage
01590   read #1,using L1740,key=key$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey L1650
01600   fnrglbig$(key$)
01610   for j=1 to 13
01620     if revb(j)=-202020202.02 then revb(j)=0
01630   next j
01640   edit_mode=1
01650 L1650: ! 
01658   goto MAIN
01660 ! /r
01670 SAVE: ! r:
01680   if holdgl$<>gl$ then ! attempting to change general ledger number
01682     mat ml$(3)
01684     ml$(1)="You are attempting to change account # "&holdgl$&"!"
01686     ml$(2)="to "&gl$&".  Take OK to change the account."
01688     ml$(3)="Take Cancel to return to main screen."
01690     fnmsgbox(mat ml$,resp$,cap$,49)
01692     if resp$<>"OK" then 
01694       goto L1780
01696     end if 
01698   end if 
01720 ! !f BB=0 Then bB=CB  ! need to make current bal & begbalance same if adding new record
01730   rewrite #1,using L1740,key=key$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
01740 L1740: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
01750   if cl1=1 then 
01752     rewrite #8,using L1740,key=key$: gl$,d$ nokey L1760
01754   end if 
01760 L1760: ! 
01762   if gl$<>holdkey$ then 
01764     gosub CHG_GLNO_IN_HISTORY ! change account numbers in history
01770     gosub CHANGE_CURRENT_TRANS
01772   end if
01780 L1780: ! 
01781   bb=cb=pbp=0
01782   mat bc=(0) : mat bp=(0) : mat bm=(0) : mat revb=(0) : mat ta=(0) : mat rf=(0)
01784   edit_mode=0
01786   gl$=""
01790   goto MAIN
01800 ! /r
01810 DELETE_ACCT: ! r:
01820   if cb=0 then goto L1880
01830   mat ml$(3) 
01840   ml$(1)="Account # "&gl$&" has a balance. You should not " 
01850   ml$(2)="delete an account with a balance." 
01860   ml$(3)="Take OK to delete; else Cancel to return to main screen." 
01870   fnmsgbox(mat ml$,resp$,cap$,49)
01877   if resp$="OK" then delete_it=1: goto L1910
01878   if resp$="Cancel" then goto L2030
01880 L1880: !
01882   mat ml$(3)
01884   ml$(1)="You have chosen to delete account # "&gl$&"!" 
01886   ml$(2)="Take OK to delete the account." 
01888   ml$(3)="Take Cancel to return to main screen." 
01890   fnmsgbox(mat ml$,resp$,cap$,49)
01892   if resp$="OK" then delete_it=1: goto L1910
01900   if resp$="Cancel" then goto L2030
01910 L1910: !
01912   delete #1,key=gl$: ioerr L2030
01920   if cl1=1 then delete #8,key=gl$: nokey ignore
01922   goto CHANGE_CURRENT_TRANS ! /r
01930 CHANGE_CURRENT_TRANS: ! r:
01940   if ta(1)=0 then goto L2030
01950   adr=ta(1)
01960 L1960: read #2,using L2800,rec=adr: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
01970   if delete_it=1 then goto L1980 else goto L2000
01980 L1980: rewrite #2,using L2800,rec=adr: "",0,0,0,0,"","",0
01990   goto L2010
02000 L2000: rewrite #2,using L2800,rec=adr: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
02010 L2010: if nta=0 then goto L2030
02020   adr=nta : goto L1960
02030 L2030: delete_it=0
02040   return  ! /r
02050 ! PRINT_PROOF: ! r:  pr proof list is currently unused
02060 !   restore #1,key>="            ": eof ignore
02070 !   fnwait("Printing: Please wait...",1)
02080 ! ! on fkey 5 goto PP_FINIS
02090 !   fnopenprn
02100 !   gosub PP_HEADER
02110 ! PP_READ_1: ! 
02112 !   read #1,using L1740: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp eof PP_FINIS
02120 !   read #1,using L1740: gl2$,de$,mat rf2,d,c,mat e,mat h,mat g,pb eof PP_EOF_1
02130 !   goto PP_DETAILS ! /r
02140 ! PP_HEADER: ! r:
02142 !   pr #255,using L2150: date$('mm/dd/yy'),time$,env$('cnam'),"G/L Proof List",date$("Month DD, CCYY")
02150 ! L2150: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,cc 50,skip 1,pos 59,c 30,skip 1,cc 66,skip 2
02160 !   return  ! /r
02162 ! PP_EOF_1: ! r:
02163 !   c=d=pb=0
02164 !   de$="0"
02165 !   mat e=(0) : mat h=(0) : mat g=(0)
02166 !   goto PP_DETAILS ! /r
02170 ! PP_DETAILS: ! r: some printing stuff
02180 !   pr #255,using L2190: gl$,gl2$
02190 ! L2190: form pos 1,c 13,pos 69,c 13,skip 2
02200 !   pr #255,using L2210: "Description",d$,"Description",de$
02210 ! L2210: form pos 1,c 12,c 50,pos 69,c 12,c 50,skip 2
02220 !   pr #255,using L2230: "Beginning Balance",bb,"Current Balance",cb,"Beginning Balance",d,"Current Balance",c
02230 ! L2230: form pos 1,c 18,pic(---------.##),x 4,c 16,pic(---------.##),pos 69,c 18,pic(---------.##),x 4,c 16,pic(---------.##),skip 2
02240 !   pr #255,using L2250: "Balance Sheet #",rf(1),"2nd Balance Sheet #",rf(2),"Balance Sheet",rf2(1),"2nd Balance Sheet #",rf2(2)
02250 ! L2250: form pos 1,c 20,n 5,x 9,c 23,n 5,pos 69,c 20,n 5,x 9,c 23,n 5,skip 1
02260 !   pr #255,using L2250: "Income Statement",rf(3),"2nd Income Statement",rf(4),"Income Statement",rf2(3),"2ND Income Statement",rf2(4)
02270 !   pr #255,using L2250: "Fund Statement  ",rf(5),"2nd Fund Statement",rf(6),"Fund Statement  ",rf2(5),"2nd Fund Statement",rf2(6)
02280 !   pr #255,using L2290: "EOY Balance 2 Yr Ago",pbp,"EOY Balance 2 Yr Ago",pb
02290 ! L2290: form pos 1,c 20,pic(--------.##),pos 69,c 20,pic(--------.##),skip 2
02300 !   pr #255,using L2310: "Current Year","Prior Year","Budget","Current Year","Prior Year","Budget"
02310 ! L2310: form skip 2,pos 12,c 16,c 19,c 12,pos 80,c 16,c 19,c 12,skip 1
02320 !   for j=1 to 13
02330 !     pr #255,using L2340: "Period",j,bc(j),bp(j),bm(j),"Period",j,e(j),h(j),g(j)
02340 ! L2340: form pos 1,c 6,n 3,pic(------------.##),pic(-----------.##),pic(------------.##),pos 69,c 6,n 3,pic(------------.##),pic(-----------.##),pic(------------.##),skip 1
02350 !   next j
02360 !   pr #255,using L2370: ""
02370 ! L2370: form c 1,skip 2
02380 !   cn=cn+1
02390 !   if cn<>2 then goto PP_READ_1
02410 !   pr #255: ""
02412 !   pr #255: ""
02420 !   cn=0
02430 !   gosub PP_HEADER
02440 !   goto PP_READ_1
02450 ! PP_FINIS: ! 
02452 !   fncloseprn
02460 !   on fkey 5 ignore 
02470 !   if fnprocess=1 then goto XIT else goto MAIN
02480 ! ! /r
02550 IGNORE: continue 
02560 REVIEW_TRANS: ! r:
02570   fntos(sn$="review_trans")
02572   mylen=23: mypos=mylen+3 : right=1: rc=0
02580   fnfra(1,1,3,50,"Review transactions","Transactions are retained in the current files until the month is closed; then they are transferred to history ",0)
02590   fnopt(1,1,"Current transactions",0,1)
02600   resp$(1)="True"
02610   fnopt(2,1,"History transactions",0,1)
02620   resp$(2)="False"
02630   fncmdset(2)
02640   fnacs(sn$,0,mat resp$,ckey)
02650   if ckey=5 then goto MAIN
02660   if resp$(1)="True" then rv=1
02670   if resp$(2)="True" then rv=2
02680 ! ______________________________________________________________________
02690 TRANSACTION_GRID: ! 
02692   mat chdr$(9) : mat cmask$(9) : mat item$(9) !:
        chdr$(1)='Ref': chdr$(2)='G/L #': chdr$(3)='Date' !:
        chdr$(4)='Amount' !:
        chdr$(5)='T Code' : chdr$(6)='P Code' !:
        chdr$(7)='Ck/Rec #' : chdr$(8)='Description' !:
        chdr$(9)='Period'
02700   cmask$(1)="30"
02702   cmask$(2)="": cmask$(3)="3" : cmask$(4)='10' !:
        cmask$(5)='30' : cmask$(6)='30': cmask$(7)='' !:
        cmask$(8)='' : cmask$(9)='30'
02710   fntos(sn$="gltrans")
02720   fnflexinit1('Currentfile',1,1,20,85,mat chdr$,mat cmask$,1,0)
02730   adr=ta(1): pc2=0
02740 !  read current or history files
02750   if rv=1 then goto READ_FROM_CURRENT else goto READ_FROM_HISTORY
02760 READ_FROM_CURRENT: ! 
02770   transfile=2
02780 L2780: if adr=0 then goto EO_TRANS_GRID
02790   read #2,using L2800,rec=adr,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
02800 L2800: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
02810   adr=nta
02820   goto DISPLAY_TRANS
02830 READ_FROM_HISTORY: ! 
02840   transfile=3
02850   ack$=gl$&cnvrt$("N 2",pc1)&"      "
02860   restore #3,key>=ack$: nokey EO_TRANS_GRID
02870 L2870: read #3,using L2880,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof EO_TRANS_GRID
02880 L2880: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
02890   if key$><trgl$ then goto EO_TRANS_GRID
02900   if pc1=0 then goto L2930
02910   if pc1><pc2 then goto L2870
02920 DISPLAY_TRANS: ! 
02930 L2930: item$(1)=str$(rec(transfile))
02932   item$(2)=trgl$: item$(3)=str$(tr(4)): item$(4)=str$(tr(5)) !:
        item$(5)=str$(tr(6)) : item$(6)=str$(tr(7)) : item$(7)=tr$ !:
        item$(8)=td$: item$(9)=str$(pc2) !:
        fnflexadd1(mat item$)
02940   if rv=1 then goto L2780 else goto L2870 ! read from current or history
02950 EO_TRANS_GRID: ! 
02960   if rv=1 then let fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any information.")
02970   fncmdkey("E&xit",5,0,1,"Exits to main menu")
02980   fnacs(sn$,0,mat resp$,ck)
02990   if ck=5 then goto MAIN
03000   if ck=2 then edit_mode=1 else edit_mode=0
03010   recordnum=val(resp$(1))
03020   if recordnum=0 then goto MAIN
03030   if rv=1 then read #2,using L2800,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta else read #3,using L3350,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
03040   resp$(1)=str$(recordnum): resp$(2)=trgl$: resp$(3)=str$(tr(4))
03042   resp$(4)=str$(tr(5)): resp$(5)=str$(tr(6)) : resp$(6)=str$(tr(7)) !:
        resp$(7)=tr$: resp$(8)=td$ !:
        resp$(9)=str$(pc2)
03050   fntos(sn$="Tredit")
03052   mylen=23: mypos=mylen+3 : right=1
03060   fnlbl(1,1,"General Ledger Number:",mylen,right)
03070   fnqglbig(1,mypos,0,2)
03072   resp$(1)=fnrglbig$(trgl$)
03080   holdgl$=gl$
03090   fnlbl(2,1,"Date:",mylen,right)
03100   fntxt(2,mypos,10,0,right,"1",0,"",0)
03102   resp$(2)=str$(tr(4))
03110   fnlbl(3,1,"Amount:",mylen,right)
03120   fntxt(3,mypos,10,0,right,"10",0,"",0)
03122   resp$(3)=str$(tr(5))
03130   fnlbl(4,1,"Trans Code:",mylen,right)
03140   fntxt(4,mypos,2,0,right,"30",0,"Transaction Code - 1=Disbursment  2= Receipt  3= Adjustment",0)
03142   resp$(4)=str$(tr(6))
03150   fnlbl(5,1,"Post Code:",mylen,right)
03160   fntxt(5,mypos,2,0,right,"30",0,"",0)
03162   resp$(5)=str$(tr(7))
03170   fnlbl(6,1,"Ck/Ref:",mylen,right)
03180   fntxt(6,mypos,12,0,right,"",0,"",0)
03182   resp$(6)=tr$
03190   fnlbl(7,1,"Description:",mylen,right)
03200   fntxt(7,mypos,30,0,0,"",0,"",0)
03202   resp$(7)=td$
03210   fnlbl(8,1,"Period:",mylen,right)
03220   fntxt(8,mypos,2,0,0,"30",0,"",0)
03222   resp$(8)=str$(pc2)
03230   fncmdset(2)
03240   fnacs(sn$,0,mat resp$,ckey)
03250   if ckey=5 then goto MAIN
03260   trgl$=fnagl$(resp$(1)) ! transaction gl #
03270   tr(4)=val(resp$(2)) ! date
03280   tr(5)=val(resp$(3)) ! amount
03290   tr(6)=val(resp$(4)) ! t code
03300   tr(7)=val(resp$(5)) ! p code
03310   tr$=resp$(6) ! reference #
03320   td$=resp$(7) ! reference #
03330   pc2=val(resp$(8)) ! period code from history; blank when returning from current
03340   if rv=1 then rewrite #2,using L2800,rec=recordnum: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta else rewrite #3,using L3350,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
03350 L3350: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
03360   adr=ta(1): goto TRANSACTION_GRID
03370 ! /r
03630 SEARCH_GRID: ! r:
03640   fnaccount_search(gl$,fixgrid)
03650   fixgrid=0
03660   read #1,using L1740,key=gl$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey MAIN
03670   goto MAIN ! need search grid here   KJ
03680 ! /r
03690 ! <Updateable Region: ERTN>
03700 ERTN: fnerror(program$,err,line,act$,"xit")
03710   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03720   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03740 ERTN_EXEC_ACT: execute act$ : goto ERTN
03750 ! /region
03760 ! ______________________________________________________________________
03770 CHG_GLNO_IN_HISTORY: ! r: change gl # in history
03780   ack$=holdgl$&cnvrt$("N 2",0)&"      "
03785   if trim$(ack$)="" then goto L3850
03790   restore #3,key>=rpad$(ack$,kln(3)): nokey L3850
03800   do
03802     read #3,using L3810: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof L3850
03810     L3810: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
03820     if holdgl$=trgl$ then 
03830       rewrite #3,using L3810: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
03832     end if
03840   loop
03850 L3850: !
03860 return ! /r
03870 CHANGE_ACCT_NUM: ! r:
03880   dno=val(gl$(1:3)): ano=val(gl$(4:9)): sno=val(gl$(10:12))
03890   mat resp$=("")
03900   fntos(sn$="GLchange")
03902   mylen=28: mypos=mylen+3 : right=1: rc=0
03910   if use_dept =1 then let fnlbl(1,31,"Fund #",6,2)
03920   if use_sub =1 then let fnlbl(1,45,"Sub #",6,2)
03930   fnlbl(2,1,"New General Ledger Number:",mylen,right)
03940   if use_dept=1 then 
03941     fntxt(2,31,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 )
03942     resp$(rc+=1)=str$(dno)
03946   end if
03950   fntxt(2,36,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 )
03952   resp$(rc+=1)=str$(ano)
03960   if use_sub=1 then 
03962     fntxt(2,45,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) 
03964     resp$(rc+=1)=str$(sno)
03966   end if
03970   fnlbl(3,1,"Description:",mylen,right)
03980   fntxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
03982   resp$(rc+=1)=d$
03990   fncmdset(2)
04000   fnacs(sn$,0,mat resp$,ckey)
04020   if ckey=5 then goto MAIN
04030   fixgrid=99
04040   dno=ano=sno=0
04050   if use_dept=1 then dno=val(resp$(1)) : ano=val(resp$(2))
04060   if use_dept=0 then ano=val(resp$(1))
04070   if use_dept=1 and use_sub=1 then sno=val(resp$(3))
04080   if use_dept=0 and use_sub=1 then sno=val(resp$(2))
04090   if use_dept=1 and use_sub=1 then d$=resp$(4)
04100   if use_dept=0 and use_sub=1 then d$=resp$(3)
04110   if use_dept=0 and use_sub=0 then d$=resp$(2)
04120   if use_dept=1 and use_sub=0 then d$=resp$(3)
04130   key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
04140   if key$=gl$ then goto L4200 ! only changing description
04150   read #1,using 'Form POS 1,N 3',key=key$: dno nokey L4190
04160 ! MSGBOX5: !
04170   mat ml$(3)
04172   ml$(1)="General ledger account # "&key$&" already " 
04174   ml$(2)="exists. Take OK to review the account." 
04176   ml$(3)="Take Cancel to return to main screen." 
04178   fnmsgbox(mat ml$,resp$,cap$,49)
04180   if resp$="OK" then goto DO_EDIT else goto MAIN
04190 L4190: !
04192   gl$=key$
04200 L4200: !
04202   rewrite #1,using L4210,key=holdgl$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
04210 L4210: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
04220   if cl1=1 then 
04222     rewrite #8,using L4210,key=holdgl$: gl$,d$ nokey L4230
04224   end if
04230 L4230: !
04234   gosub CHG_GLNO_IN_HISTORY ! change account numbers in history
04240   gosub CHANGE_CURRENT_TRANS
04250   goto MAIN ! /r
20000 MSGBOX4: ! r:
20020   mat ml$(4)
20040   ml$(1)="You cannot change an account number in this manner!"
20060   ml$(2)="Take the 'Change #' option to change either the"
20080   ml$(3)="account number or the description."
20100   ml$(4)="Click OK to access the new account; else Cancel to quit."
20120   fnmsgbox(mat ml$,resp$,cap$,49)
20140   if resp$='OK' then goto DO_EDIT
20160   if resp$='Cancel' then gl$=key$=holdgl$: goto MAIN ! /r
