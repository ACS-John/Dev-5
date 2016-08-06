00010 ! R:\acsCL\unpaidinvoice
00020   library 'R:\Core\Library': fntop,fnxit, fncno,fnopenprn,fncloseprn,fnchain,fnerror,fntop,fnxit,fndate_mmddyy_to_ccyymmdd,fntos,fnfra,fnlbl,fntxt,fncombof,fncomboa,fnbutton,fncmdkey,fnacs,fnmsgbox,fnflexadd1,fnflexinit1,fnchk,fnaddpayee,fnqgl,fnagl$,fnrgl$,fnjob_srch,fncmbjob,fncmbcategory
00030   library 'R:\Core\Library': fngethandle,fncmbsubcat,fncategory_srch,fnclient$,fncd,fnregistered_for_job_cost_pr
00040   let fntop(program$,cap$="Unpaid Invoice")
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim n$*40,a$(3)*30,b(4),k$*20,hk$*20,k4$*20,cap$*128
00080   dim jobdesc$*30,jn$*6,l(11),ta(2),jobname$*25,jobitem$(6)*30
00090   dim bn$*30,bcn$*30,in1$(9),de$*30,ref(20),ta(2)
00100   dim gl(3),pr$(4)*30,t1(5),ink$(20),up$(4),unpaidkey$*20
00110   dim nam$*50,cnam$*40,d(2),sn$*50,egl(10)
00120   dim bk$(20)*28,nam$*28,cnt$*25,jci$(100)*30
00130   dim rn$*12,jn$*6,ji2(3),cn$*11,l(13),jta(2),empnum$*12,empnam$*30
00140   dim gd1$(99)*30,gd2$(99)*30 ! used with file #7
00150   dim contact$*30,ph$*12,email$*50,fax$*12,myact$*20,resp$(50)*50
00160   dim chdr$(16),cmask$(16),item$(16)*21 ! used with flex grid
00170   dim gldesc$*30,apc(99,3),gi1(99,4),gi2(99,4),td$*30,ml$(3)*80
00180   dim holdresp$(256)*50,item1$(3)*15,type$*25,holdkey$*20,resp$(256)*50
00190 ! ______________________________________________________________________
00200   let screen_last=5
00210 ! ______________________________________________________________________
00220   let cancel=99 : let right=1 : let center=2
00230   let fncno(cno,cnam$)
00240   open #20: "Name=Q:\CLmstr\PostDat.h"&str$(cno)&",Shr,Use,RecL=12",internal,outin,relative 
00250   read #20,using 'Form POS 1,2*N 6',rec=1: dt1,dt2 norec L690
00260   goto L700
00690 L690: ! 
00700   write #20,using 'Form POS 1,2*N 6',rec=1: dt1,dt2
00705 L700: ! 
00710   close #20: 
00715   open #20: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00720   read #20,using 'Form POS 150,2*N 1,C 2',rec=1: mat d,bc$
00725   close #20: 
00730   let bankcode=val(bc$)
00735   open #glmstr=11: "Name=Q:\CLmstr\GLmstr.H"&str$(cno)&",KFName=Q:\CLmstr\GLIndex.H"&str$(cno)&",Shr",internal,outin,keyed 
00740   open #bankmstr=12: "Name=Q:\CLmstr\BankMstr.H"&str$(cno)&",KFName=Q:\CLmstr\BankIdx1.H"&str$(cno)&",Shr",internal, outin, keyed 
00745   read #bankmstr,using 'Form POS 3,C 30',key=bc$,release: bcn$ nokey L760
00760 L760: ! 
00770   open #paymstr1=13: "Name=Q:\CLmstr\PayMstr.H"&str$(cno)&",KFName=Q:\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00780   open #paymstr2=14: "Name=Q:\CLmstr\PayMstr.H"&str$(cno)&",KFName=Q:\CLmstr\PayIdx2.H"&str$(cno)&",Shr",internal,outin,keyed 
00790   open #payeegl=17: "Name=Q:\CLmstr\payeeGLBreakdown.h"&str$(cno)&",KFName=Q:\CLmstr\Payeeglbkdidx.h"&str$(cno)&",Shr",internal,outin,keyed 
00800   open #paytrans=4: "Name=Q:\CLmstr\PayTrans.H"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00810   open #unpdaloc=5: "Name=Q:\CLmstr\UnPdAloc.H"&str$(cno)&",KFName=Q:\CLmstr\Uaidx2.H"&str$(cno)&",Shr",internal,outin,keyed 
00820   read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=bc$,release: bn$,bal,upi,lcn$ nokey L820
00825 L820: ! 
00830   let t1(1)=bal : let upi=t1(5) : let t1(3)=t1(1)-t1(2)
00835   if fnregistered_for_job_cost_pr then let havejc=1: gosub JCBLD
00840 ! ______________________________________________________________________
00845 MENU1: ! 
00850   print newpage
00855   mat chdr$(16) : mat cmask$(16) : mat item$(16)
00860   let chdr$(1)='Ref': let chdr$(2)='Payee': let chdr$(3)='Invoice'
00865   let chdr$(4)='Date'
00870   let chdr$(5)='Due Date' : let chdr$(6)='P O #'
00875   let chdr$(7)='Description' : let chdr$(8)='Amount'
00880   let chdr$(9)='Disc Amt' : let chdr$(10)='Disc Date'
00885   let chdr$(11)='Pay Code' : let chdr$(12)='Bank'
00890   let chdr$(13)='Ck Num' : let chdr$(14)='Date Paid'
00895   let chdr$(15)='Post Code' : let chdr$(16)='Post Date'
00900   let cmask$(1)="30"
00905   let cmask$(2)="": let cmask$(3)="" : let cmask$(4)='1'
00910   let cmask$(5)='1' : let cmask$(6)='': let cmask$(7)=''
00915   let cmask$(8)='10' : let cmask$(9)='10' : let cmask$(10)='1'
00920   let cmask$(11)='30': let cmask$(12)='30'
00925   let cmask$(13)='30': let cmask$(14)='3'
00930   let cmask$(15)='30': let cmask$(16)='1'
00935 DISPLAY_INVOICE_GRID: ! 
00940   let fntos(sn$="unpaid1")
00945   let respc=0
00950   let frame=0
00955   let fnflexinit1('UnpaidFile',1,1,20,85,mat chdr$,mat cmask$,1,0)
00960   restore #paytrans: 
00965 READ_INVOICE_GRID: ! read unpaid invoice file
00970   read #paytrans,using 'Form POS 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof EO_INVOICE_GRID norec L970
00975   let item$(1)=str$(rec(paytrans))
00980   let item$(2)=vn$: let item$(3)=iv$: let item$(4)=up$(1)
00985   let item$(5)=up$(2) : let item$(6)=up$(3) : let item$(7)=up$(4)
00990   let item$(8)=str$(upa) : let item$(9)=str$(disamt) : let item$(10)=str$(ddate)
00995   let item$(11)=str$(pcde) : let item$(12)=str$(bcde)
01000   let item$(13)=str$(ckn) : let item$(14)=str$(dp)
01005   let item$(15)=str$(gde) : let item$(16)=str$(pdte)
01010   let fnflexadd1(mat item$)
01015   let transactionstotal+=upa
01017 L970: ! 
01019   goto READ_INVOICE_GRID
01021 EO_INVOICE_GRID: ! 
01023   if havejc=1 then let fncmdkey("&Review Job Cost Entries",9,0,0,"Allows you to review and/or post any job cost allocations you have made.")
01025   let fncmdkey("&Add",1,0,0,"Allows you to add new unpaid invoice records.")
01027   let fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing unpaid invoice record.")
01029   let fncmdkey("&Select to Pay",8,0,0,"Allows you to code invoices for payment")
01031   let fncmdkey("&Listing",3,0,0,"Prints listings from unpaid file")
01033   let fncmdkey("E&xit",5,0,1,"Exits to main menu")
01035   let fnacs(sn$,0,mat resp$,ck)
01037   let displayalljobs=0
01039   if ck=5 then goto XIT
01041   let screen=0
01043   if ck=2 then let edit=1 else let edit=0
01045   if ck=3 then goto PRINTLISTING ! print listings of unpaid invoice file
01047   if ck=8 then goto CODE_FOR_PAYMENT ! select invoices to payment
01049   if ck=9 then 
01051     let displayalljobs=1
01053     let jn$="" : let iv$="" : let vn$=""
01055     let subcat=0 : let cat=0
01057     gosub JOBCOST
01059     goto DISPLAY_INVOICE_GRID
01061   end if 
01063   if ck=1 then goto ADD_UNPAID_INVOICES ! add
01065   if ck=2 then 
01067     read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=val(resp$(1)),release: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
01069     mat resp$=("")
01071     let holdkey$=vn$&iv$
01073   else 
01075     let holdkey$=''
01077   end if 
01079   let resp$(1)=vn$: let resp$(2)=iv$: let resp$(3)=up$(1)
01081   let resp$(4)=up$(2): let resp$(5)=up$(3) : let resp$(6)=up$(4)
01083   let resp$(7)=str$(upa): let resp$(8)=str$(disamt)
01085   let resp$(9)=str$(ddate) : let resp$(10)=str$(pcde)
01087   let resp$(11)=str$(bcde)
01089   let resp$(12)=str$(ckn) : let resp$(13)=str$(pd)
01091   let resp$(14)=str$(gde): let resp$(15)=str$(pdte)
01093 READGLBREAKDOWN: ! 
01095   let x=11
01097   mat holdresp$=("")
01099 ! start with +=1 11 to match the next screen which does not some of above information
01101   restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey EO_READGLBREAKDOWN
01190 L1190: read #unpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$ eof EO_READGLBREAKDOWN
01200   if vn$=hvn$ and iv$<>hiv$ then goto L1190
01210   if vn$<>hvn$ or iv$<>hiv$ then goto EO_READGLBREAKDOWN
01220   if x>40 then goto L1250
01230   let resp$(x+=1)=gl$
01240   let resp$(x+=1)=str$(percent)
01250   let resp$(x+=1)=gldesc$
01260   goto L1260
01265 L1250: ! 
01270   let holdresp$(x+=1)=gl$
01275   let holdresp$(x+=1)=str$(percent)
01280   let holdresp$(x+=1)=gldesc$
01282 L1260: ! 
01284   goto L1190
01286 ! ___________________________
01288 EO_READGLBREAKDOWN: ! 
01290   goto ADD_UNPAID_INVOICES_2 ! edit
01292 ! ______________________________________________________________________
01294 PRINTLISTING: ! r: print listings
01296 ! need screen here asking paid/unpaid/all,starting inv #,1st inv to print,show breakdowns(y/n),display last ref in file, 1st ref # used this time
01298 ! need ref # to begin print (blank for all)   rf2    show last ref  lrec(4)
01300   let fnopenprn
01302   let pg=0
01304   if rf2=0 then let rf2=1
01306   gosub HDR
01308   let t1=0
01310   for j=rf2 to lrec(4)
01312     read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,G 2,G 8,G 6,N 1',rec=j,release: mat in1$,ckn,dp,gde norec NEXTRECORD
01314     if rtrm$(in1$(1))="" then goto NEXTRECORD
01316     let t0=val(in1$(7)) conv L1440
01318     let t1=t1+t0
01440 L1440: ! 
01450     print #255,using 'Form POS 1,N 4,X 2,C 10,C 14,2*C 8,C 14,C 20,C 12,C 6,G 4,N 10,N 8': j,mat in1$,ckn,dp pageoflow NEWPGE
01460     let aa2=0
01470     let r5=aa(1)
01480     restore #unpdaloc,key>=in1$(1)&"            ": nokey NEXTRECORD
01485 L1480: ! 
01490     read #unpdaloc,using 'Form POS 1,c 8,c 12,c 12,PD 5.2,C 30',release: vnkey$,vniv$,gl$,aa,de$ eof NEXTRECORD
01495     if in1$(1)<>vnkey$ then goto NEXTRECORD ! not same vendor
01500     if in1$(2)<>vniv$ then goto L1480 ! not same invoice
01505     let aa2=aa2+aa
01510     print #255,using 'Form POS 47,c 12,X 2,C 20,N 10.2': gl$,de$(1:20),aa pageoflow NEWPGE
01515     goto L1480
01520     print #255: 
01525 NEXTRECORD: ! 
01530   next j
01535   print #255,using 'Form POS 81,C 10,SKIP 1,POS 81,N 10.2,SKIP 1,POS 81,C 10,SKIP 1': "__________",t1,"=========="
01540 ! 
01545   let fncloseprn
01550   on fkey 99 ignore 
01555   goto DISPLAY_INVOICE_GRID ! /r
01560 ! ______________________________________________________________________
01565 NEWPGE: print #255: newpage: gosub HDR : continue 
01570 ! ______________________________________________________________________
01575 HDR: ! r:
01580   let pg=pg+1
01585   let fnopenprn
01590   print #255,using 'Form POS 1,C 8,Cc 82': date$,cnam$
01595   print #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg,"Unpaid Invoice File Listing"
01600   print #255: ""
01605   print #255: "                             Invoice    Due     PO Number                                   Pay   Bank   Check     Date "
01610   print #255: "Ref#  Payee #   Invoice Numb   Date    Date     GL Number   Description            Amount   Code  Code   Number    Paid "
01615   print #255: "____  ________  ____________  ______  ______  ____________  __________________  __________  ____  ____  ________  ______"
01620   return  ! /r
01625 ! ______________________________________________________________________
01630 DONE: ! 
01635   close #glmstr: ioerr ignore
01750   close #bankmstr: ioerr ignore
01760   close #paymstr1: ioerr ignore
01770   close #paymstr2: ioerr ignore
01780   close #1: ioerr ignore
01790   close #2: ioerr ignore
01800   close #2: ioerr ignore
01810   close #paytrans: ioerr ignore
01820 ! execute "Index Q:\CLmstr\PayTrans.H"&STR$(CNO)&",Q:\CLmstr\unpdidx1.H"&STR$(CNO)&",1,20,Replace,DupKeys -n"
01830   let idx4=0
01840   if ti1=6 then let fnchain("R:\acsCL\CkPrt") else goto XIT
01850 ! ______________________________________________________________________
01860 XIT: ! 
01870   if havejc=1 and lrec(jcbreakdown)>0 then goto L1880 else goto L1900
01880 L1880: ! 
01890   mat ml$(3)=("")
01900   let ml$(1)="It appears you have "&str$(lrec(jcbreakdown))&"job cost entries"
01910   let ml$(2)="that have not been posted.  Do you wish to post these"
01920   let ml$(3)="entries before you exit?"
01930   let fnmsgbox(mat ml$,resp$,cap$,4)
01940   if resp$="Yes" then gosub POST_TO_JOB : goto XIT
01945 L1900: let fnxit
01950 ! ______________________________________________________________________
01955   execute "INDEX Q:\CLmstr\PayTrans.H"&str$(cno)&",Q:\CLmstr\UnPdIdx1.H"&str$(cno)&",1,20,Replace,DupKeys -n"
01960   execute "INDEX Q:\CLmstr\unpdaloc.H"&str$(cno)&",Q:\CLmstr\Uaidx2.H"&str$(cno)&",1,20,Replace,DupKeys -n"
01965   execute "INDEX Q:\CLmstr\unpdaloc.H"&str$(cno)&",Q:\CLmstr\Uaidx1.H"&str$(cno)&",9,12,Replace,DupKeys -n"
01970   open #paytrans=4: "Name=Q:\CLmstr\PayTrans.H"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
01975   restore #paytrans,key>="                    ": nokey L2120
01980 L1980: ! 
01990   read #paytrans,using 'Form POS 1,C 8,C 12,POS 27,N 6,POS 63,N 10.2,N 1,N 2': vn$,iv$,dd,upa,pcde,bcde eof L2120
02000   if bcde>0 then goto L1980 ! dont reselect an invoice already coded
02010   let saverec4=rec(4) : if vn$=hvn$ then goto L2030
02020   gosub ADDUP_BALANCE_OWED
02030   let hvn$=vn$
02035 L2030: ! 
02040   if t1<=0 then goto L1980 ! SKIP
02045   let dt1=fndate_mmddyy_to_ccyymmdd(dd)
02050   if dt1>pd1 then goto L1980
02055   if nopay=1 then goto L2110
02060   rewrite #paytrans,using 'Form POS 73,N 1,N 2',rec=saverec4: 1,bankcode
02065   let t1(2)=t1(2)-upa
02070   let t1(3)=t1(1)-t1(2)
02075   let t1(4)=t1(5)-t1(2)
02110 L2110: ! 
02120   goto L1980
02125 L2120: ! 
02130   close #102: ioerr ignore
02134   goto MENU1
02136   print newpage
02138   let fnopenprn
02140 ! close #106: ioerr ignore
02170 ! open #106: "SROW=08,SCOL=20,EROW=12,ECOL=59,BORDER=SR,CAPTION=Selected Invoice Listing",display,outin
02180 ! print #106: newpage
02190 ! print #106,fields "1,1,Cc 40,R,N": cnam$
02200 ! print #106,fields "2,1,Cc 40,R,N": "Company Number "&str$(cno)
02210 ! print #106,fields "4,1,Cc 40,N": "In Process..."
02220 ! print fields "13,34,C 12,B,99": "Cancel (Esc)"
02230   on fkey 5 goto L2620
02240   restore #paytrans,key>="                    ": nokey L2620
02250   let hvn$=""
02260   let t1=pg1=0
02270   gosub HEADER
02280 L2280: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,G 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,ck$,dp,gde eof L2620
02290   let bcde=bc
02300   if rtrm$(vn$)="" then goto L2280
02310   if pcde><1 then goto L2280
02320   if bc<>bankcode then goto L2280 ! only print for current bank account
02330   if hvn$="" then goto L2350
02340   if hvn$><vn$ then gosub READ_AND_PRINT
02350 L2350: ! 
02360   form pos 1,c 10,c 12,2*pic(zzzz/zz/zz),x 2,c 18,pic(zzz,zzz,zzz.##cr),skip 1
02370   let t1=t1+upa
02380   let hvn$=vn$
02390   goto L2280
02400 ! ______________________________________________________________________
02410 PGOF: ! r:
02420   print #255: newpage
02430   gosub HEADER
02440   continue  ! /r
02460 HEADER: ! r:
02470   print #255,using 'Form POS 1,PIC(ZZ/ZZ/ZZ),CC 107': prd,cnam$
02480   let pg1=pg1+1
02490   print #255,using 'Form POS 1,C 4,N 4,CC 107,SKIP 1,POS 1,C 8': "Page",pg1,"Selected Invoice Listing for Bank"&ltrm$(str$(bankcode)),date$
02500   print #255: "  Payee                  Invoice     Due                                                   Line Item                       Item"
02510   print #255: " Number   Invoice Numb    Date      Date    Inv. Description       Amount     GL Number    Description                    Amount"
02520   print #255: "________  ____________  ________  ________  __________________  ____________  ____________ ______________________________ __________"
02530   return  ! /r
02550 READ_AND_PRINT: ! r:
02560   let sn$=""
02570   read #paymstr1,using 'Form POS 9,4*C 30,POS 147,2*PD 3',key=hvn$,release: sn$ nokey L2580
02580 L2580: ! 
02590   print #255,using 'Form POS 65,C 14,SKIP 1,POS 30,C 33,PIC(ZZZ,ZZZ,ZZZ.##CR)': "____________",sn$,t1 pageoflow PGOF
02600   print #255: "" pageoflow PGOF
02610   let t1=0
02620   return  ! /r
02635 L2620: ! 
02640   return 
02645 ! ______________________________________________________________________
02700 JCBLD: ! r: Open JC Files
02705   mat chdr3$(6) : mat cmask3$(6) : mat jobitem$(6)
02710   let chdr3$(1)='Refenence': let chdr3$(2)='Job #'
02715   let chdr3$(3)='Cat #': let chdr3$(4)='Sub-Cat #'
02720   let chdr3$(5)='Amount': let chdr3$(6)='Description'
02725   let cmask3$(1)=cmask3$(2)=cmask3$(3)=cmask3$(4)=''
02730   let cmask3$(5)='10' : let cmask3$(6)=''
02735   open #41: "Name=Q:\PRmstr\JCMSTR.H"&str$(cno)&",KFName=Q:\PRmstr\JCIndx.H"&str$(cno)&",Shr",internal,outin,keyed ioerr L2840
02740   open #category:=2: "Name=Q:\PRmstr\JCCAT.H"&str$(cno)&", KFName=Q:\PRmstr\CATIndx.H"&str$(cno)&",Shr", internal,outin,keyed 
02745   open #43: "Name=Q:\PRmstr\SCMSTR.H"&str$(cno)&",KFName=Q:\PRmstr\SCIndex.H"&str$(cno)&",Shr",internal,outin,keyed 
02750   open #45: "Name=Q:\PRmstr\JCTrans.H"&str$(cno)&",Shr",internal,outin,relative 
02755   if exists("Q:\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno)) then goto L2820 else gosub L6940
02820 L2820: open #jcbreakdown=46: "Name=Q:\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno)&",KFName=Q:\CLmstr\JcBrkidx"&wsid$&".h"&str$(cno)&",Version=1,Shr",internal,outin,keyed ioerr L6940
02830   let jc1=1
02840 L2840: ! 
02850   return  ! /r
02870 ADDUP_BALANCE_OWED: ! r:
02880   let t1=0
02890   let dt1=fncd(dd)
02900   if ti1><4 then goto L2920
02910   if dt1>pd1 and pd1>0 then goto L2930
02920 L2920: let t1=upa
02930 L2930: read #paytrans,using 'Form POS 1,C 8,POS 27,N 6,POS 63,N 10.2,N 1',release: v2$,d2,a2,pcd2 eof L3000
02940   if v2$><vn$ then goto L3000
02950   if ti1><4 then goto L2980
02960   let dt2=fndate_mmddyy_to_ccyymmdd(d2)
02970   if dt2>pd1 then goto L2990
02980 L2980: let t1=t1+a2
02990 L2990: goto L2930
03000 L3000: restore #paytrans,search>=vn$: nokey L3020
03010   read #paytrans,using 'Form POS 1,C 8,POS 63,N 10.2,N 1,N 2': vn$
03020 L3020: ! 
03030   return  ! /r
03040 ! <Updateable Region: ERTN>
03050 ERTN: let fnerror(cap$,err,line,act$,"xit")
03060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
03070   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
03090 ERTN_EXEC_ACT: execute act$ : goto ERTN
03100 ! /region
03110 IGNORE: continue 
03120 ADD_UNPAID_INVOICES: ! 
03130 HERE: ! 
03140   mat resp$=("") : let vn$=''
03150 ADD_UNPAID_INVOICES_2: ! 
03160 ! Let PAS=0
03170   let fntos(sn$="unpaid-2")
03180   let respc=0 : let frame_width=90
03190   let lc=0
03200   let fnfra(1,1,11,frame_width,"Unpaid Invoice  -  "&cnam$)
03210   let lc=0 : let mylen=32 : let mypos=mylen+2
03220   let frame=1
03230 ! Let FNLBL(LC+=1,1,CNAM$,40,2,0,1)
03240   let fnlbl(lc+=1,1,"Payee:",mylen,right,0,frame)
03250   let fncombof("Paymstr",lc,mypos,0,"Q:\CLmstr\paymstr.h"&str$(cno),1,8,9,30,"Q:\CLmstr\Payidx1.h"&str$(cno),1,pas, "Enter the payee number (Use the 'Add Payee' option to add a new vendor record",frame)
03260   let resp$(respc+=1)=vn$
03270   let fnlbl(lc+=1,1,"Invoice Number:",mylen,right,0,frame)
03280   let fntxt(lc,mypos,12,0,0,"",0,"",frame)
03290   let resp$(respc+=1)=resp$(2)
03300 ! had a required answer here; temporarly changed to a message box
03310   let fnlbl(lc+=1,1,"Invoice Date:",mylen,right,0,frame)
03320   let fntxt(lc,mypos,8,0,1,"mmddyy",0,"",frame)
03330   let resp$(respc+=1)=resp$(3)
03340   let fnlbl(lc+=1,1,"Due Date:",32,1,0,1)
03350   let fntxt(lc,mypos,8,0,1,"mmddyy",0,"",frame)
03360   let resp$(respc+=1)=resp$(4)
03370   let fnlbl(lc+=1,1,"P O Number:",32,1,0,frame)
03380   let fntxt(lc,mypos,12,0,0,"",0,"",frame)
03390   let resp$(respc+=1)=resp$(5)
03400   let fnlbl(lc+=1,1,"Description:",mylen,right,0,frame)
03410   let fntxt(lc,mypos,18,0,0,"",0,"",frame)
03420   let resp$(respc+=1)=resp$(6)
03430   let fnlbl(lc+=1,1,"Amount:",mylen,right,0,frame)
03440   let fntxt(lc,mypos,12,0,1,"10",0,"Enter the total invoice amount.",frame)
03450   let resp$(respc+=1)=resp$(7)
03460   let fnlbl(lc+=1,1,"Discount Amount:",mylen,right,0,frame)
03470   let fntxt(lc,mypos,12,0,1,"10",0,"Enter any discount allowed.",frame)
03480   let resp$(respc+=1)=resp$(8)
03490   let fnlbl(lc+=1,1,"Discount Date:",32,1,0,1)
03500   let fntxt(lc,mypos,10,0,1,"ccyymmdd",0,"",frame)
03510   let resp$(respc+=1)=resp$(9)
03520   let fnlbl(lc+=1,1,"Payment Code:",mylen,right,0,frame)
03530   let item1$(1)="Pay Later"
03540   let item1$(2)="Pay Now"
03550   let item1$(3)="Paid"
03560   let fncomboa("unpaid-2",lc,mypos,mat item1$,"If you choose pay now, the invoice will be coded for payment and will paid next time checks are printed.",0,1)
03570   if fnclient$="Miller Hardware" then let pcde=1
03580   if pcde=1 then let resp$(10)=item1$(2)
03590   if pcde=2 then let resp$(10)=item1$(3)
03600   if pcde=0 then let resp$(10)=item1$(1)
03610   let resp$(respc+=1)=resp$(10)
03620   let fnlbl(lc+=1,1,"Bank Code:",mylen,right,0,frame)
03630   let fncombof("bankmstr",lc,mypos,23,"Q:\CLmstr\bankmstr.h"&str$(cno),1,2,3,20,"Q:\CLmstr\bankidx1.h"&str$(cno),0,pas, "",frame)
03640   let resp$(respc+=1)=str$(bcde) ! RESP$(RESPC)
03650   if fnclient$="Miller Hardware" then let resp$(respc)="1"
03660   let fnbutton(1,80,"Add Payee",50,"Click to add a new payee record",0,0,frame)
03670   let fnfra(13,1,12,frame_width,"Breakdown Information"," ")
03680   let frame=2
03690   let fnlbl(1,1,"General Ledger                          Amount             Description",70,1,0,frame)
03700   for j=1 to 10
03710     let fnqgl(j+1,1,frame,2)
03720     let gl$=resp$(respc+=1)(1:12)
03730     let resp$(respc)=fnrgl$(gl$)
03740     let fntxt(j+1,43,12,0,1,"10",0,"",frame)
03750     let resp$(respc+=1)=resp$(respc)
03760     let fntxt(j+1,58,30,0,0,"",0,"",frame)
03770     let resp$(respc+=1)=resp$(respc)
03780   next j
03790   if screen>1 then 
03800     let fnbutton(12,74,"Back",21,"Previous breakdown screen",1,4,frame)
03810   end if 
03820   if screen<screen_last then 
03830     let fnbutton(12,80,"More",20,"Allows another screen of breakdowns",1,4,frame)
03840   end if 
03850   let pas=1 ! don't redo combo boxes
03860 ! 
03870   let fncmdkey("Save",1,1)
03880   let fncmdkey("&Allocate",2,0,0,"Automatically allocates the general ledger breakdown if payee record contains the breakdown information")
03890   let fncmdkey("&Delete",3,0,0,"Delete the invoice highlighted above")
03900   let fncmdkey("&Cancel",5,0,1,"Return to Unpaid Invoice selection (without saving)")
03910 ! need a fncmdkey to change screens for the breakdowns  (screen 1,2 or 3)
03920   let fnacs(sn$,0,mat resp$,ck)
03930   if ck=5 then let screen=0: goto MENU1
03940   if ck=3 then goto DELETEINVOICE
03950   let vn$=lpad$(rtrm$(resp$(1)(1:8)),8) ! payee number
03960   let iv$=lpad$(rtrm$(resp$(2)),12) ! invoice number
03970   let up$(1)=resp$(3) ! invoice date
03980   let up$(2)=resp$(4) ! due date
03990   let up$(3)=resp$(5) ! po number
04000   let up$(4)=resp$(6) ! description
04010   let upa=val(resp$(7)) ! amount
04020   let disamt=val(resp$(8)) ! discount amount
04030   let ddate=val(resp$(9)) ! discount date
04040   if item1$(1)=resp$(10) then let pcde=0 ! pay later
04050   if item1$(2)=resp$(10) then let pcde=1 ! pay now
04060   if item1$(3)=resp$(10) then let pcde=2 ! paid    KJ ????
04070   let bcde=val(resp$(11)(1:3))
04080   if ck=2 then goto READ_STANDARD_BREAKDOWNS
04090 STORE_RESPONSES: ! hold all 102 possible responses in holdresp$
04100   let x=0: let tac=0
04110   for j=1 to 11: let holdresp$(j)=resp$(j): next j
04120 ! hold standard first 11
04130   if screen=0 then let screen=1
04140 ! this section holds allocation screen which
04150 ! uses the same screen for multiple groups of allocations
04160   for j=12 to 41
04170     let x=j+((screen-1)*30)
04180 ! if screen=1 then let x=j ! (12-41)
04190 ! if screen=2 then let x=j+((screen-1)*30) ! +30 ! (42-71)
04200 ! if screen=3 then let x=j+((screen-1)*30) ! +60 ! (72-101)
04210 ! if screen=4 then let x=j+((screen-1)*30) ! +90 ! (102-...)
04220 ! if screen=5 then let x=j+((screen-1)*30) ! +60 !
04230     if int(j/3)=j/3 then 
04240       let holdresp$(x)=fnagl$(resp$(j))
04250     else 
04260       let holdresp$(x)=resp$(j)
04270     end if 
04280 ! .! hold all general ledger breakdowns (hold gl # using fhagl$)
04290   next j
04300   for j=13 to 101 step 3: let tac+=val(holdresp$(j)): next j ! total allocations
04310   if ck=3 then goto DELETEINVOICE: ! delete invoice and breakdowns
04320   if fntest_key(holdkey$,vn$,iv$,cap$)=2 then goto ADD_UNPAID_INVOICES_2
04330   if ck=50 then gosub SETUP_PAYEE : goto ADD_UNPAID_INVOICES
04340   if ck=20 or ck=21 then 
04350     goto ASSIGN_SCREENS
04360   else 
04370     goto ASSIGN_RESPONSES
04380   end if 
04390 ! ______________________________________________________________________
04400 ASSIGN_SCREENS: ! assign screen # based on more and back options
04410   if screen=0 then let screen=1
04420   if ck=20 and screen<screen_last then let screen+=1 : goto ASSIGN_RESPONSES
04430   if ck=20 and screen=screen_last then let screen=screen_last : goto ASSIGN_RESPONSES ! shouldn't happen
04440   if ck=21 and screen>1 then let screen=screen-1 : goto ASSIGN_RESPONSES
04450   if ck=21 and screen=1 then let screen=1 : goto ASSIGN_RESPONSES
04460 ASSIGN_RESPONSES: ! 
04470   for j=1 to 30
04480     if screen=1 then let x=j+11
04490     if screen=2 then let x=j+41
04500     if screen=3 then let x=j+71
04510     if screen=4 then let x=j+101
04520     if screen=5 then let x=j+131
04530     if int(j+2/3)=(j+2/3) then 
04540       let resp$(j+11)=fnrgl$(holdresp$(x))
04550     else 
04560       let resp$(j+11)=holdresp$(x)
04570     end if 
04580   next j
04590   if trim$(iv$)="" then gosub MSGBOX6 : goto ADD_UNPAID_INVOICES_2 ! must have an invoice number
04600   if ck=1 then goto WRITE_INVOICE
04610   goto ADD_UNPAID_INVOICES_2
04620 ! ______________________________________________________________________
04630 WRITE_INVOICE: ! write any new invoices and matching allocations
04640   if tac<>upa then goto MSGBOX5
04650   if edit=0 then 
04660     write #paytrans,using 'Form POS 1,Cr 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
04670   else if edit=1 then 
04680     rewrite #paytrans,using 'Form POS 1,Cr 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=rec(paytrans): lpad$(rtrm$(vn$),8),lpad$(rtrm$(iv$),12),mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
04690   end if 
04700   if edit=0 then goto L4210
04705 L4190: ! 
04710   delete #unpdaloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey L4210 eof L4210 ! delete any previous allocations because all we know about them is the invoice # and vendor#(can't identify which ones to update. change or add
04715   goto L4190
04717 L4210: ! 
04719   for j=12 to 101 step 3
04721     if val(holdresp$(j+1))<>0 then ! trim$(holdresp$(j+1))<>""
04722 ! pr vn$,iv$,holdresp$(j)(1:12),val(holdresp$(j+1)),holdresp$(j+2)(1:30) : pause
04723       write #unpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': vn$,iv$,holdresp$(j)(1:12),val(holdresp$(j+1)),holdresp$(j+2)(1:30)
04725     end if 
04727   next j
04729   if havejc=1 then gosub JOBCOST
04731   if edit=1 then let edit=0: goto MENU1
04733   mat holdresp$=(""): goto ADD_UNPAID_INVOICES ! setup up new invoice  kj 11609
04735 ! ______________________________________________________________________
04737 DELETEINVOICE: ! delete invoice
04739   delete #paytrans,key=holdkey$: ioerr L4330
04741 L4310: ! 
04743   delete #unpdaloc,key=holdkey$: nokey L4330 eof L4330 ! delete any  allocations
04745   goto L4310 ! delete all allocations
04747 L4330: goto MENU1
04749 ! ______________________________________________________________________
04751 READ_STANDARD_BREAKDOWNS: ! pull standard gl breakdowns from payee file
04753   if ck<>2 then goto L4530 ! skip automatic allocation
04755   read #paymstr1,using "form pos 1,c 8,4*c 30,pd 5.2,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20",key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey ADD_UNPAID_INVOICES_2
04757   mat holdresp$=("")
04759   let totalalloc=0
04761   restore #payeegl,key>=vn$: nokey EO_READSTGL
04763   for j=12 to 101 step 3
04765 L4420: read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_READSTGL
04767     if vn$<>payeekey$ then goto EO_READSTGL
04769     if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto L4490
04771     read #glmstr,using 'Form POS 13,C 30',key=payeegl$,release: de$ nokey L4420
04773     let holdresp$(j)=payeegl$
04775     let holdresp$(j+1)=str$(round(upa*percent*.01,2)): let totalalloc+=val(holdresp$(j+1))
04777     let holdresp$(j+2)=gldesc$ ! description
04779 L4490: next j
04781 EO_READSTGL: if totalalloc<>upa then let holdresp$(13)=str$(val(holdresp$(13))+upa-totalalloc)
04783   for j=12 to 41: let resp$(j)=holdresp$(j): next j ! put first 30 breakdowns back into resp$(
04785   if trim$(resp$(6))="" then let resp$(6)=resp$(14) ! use 1st allocation description as invoice description in none entered
04787 L4530: goto ADD_UNPAID_INVOICES_2
04789 ! ______________________________________________________________________
04791 SETUP_PAYEE: ! 
04793   let vn$=nam$=adr1$=adr2$=ss$=ph$=contact$=email$=fax$=myact$=""
04795   let type=0
04797   let pas=0
04799   let fnaddpayee
04801   return 
04803 ! ______________________________________________________________________
04805 CODE_FOR_PAYMENT: ! 
04807 KEN: ! 
04809   let lastrec=nextrec=total=0
04811   let displayattop$="True"
04813   close #clearing: ioerr L4660
04815 L4660: open #clearing=89: "Name=Q:\CLmstr\clearing.H"&wsid$&",replace,RecL=114",internal,outin,relative  ! kj wrong recl
04817   if displayunpaid=1 then 
04819     let type$="Coded for Payment"
04821   else if displayunpaid=0 then 
04823     let type$="Approved and Unapproved"
04825   else if displayunpaid=2 then 
04827     let type$="Not Approved for Payment"
04829   end if 
04831   close #paytrans: ioerr L4690
04833 L4690: open #paytrans=4: "Name=Q:\CLmstr\PayTrans.H"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
04835 L4700: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof DISPLAY_GRID
04837   if displayunpaid=1 and pcde=1 then goto L4760 ! if only choose selected, don't allow others to list
04839   if displayall=1 then goto L4760
04841   if displayunpaid=2 and pcde=0 then goto L4760 ! if only choose selected, don't allow others to list
04843   if displayunpaid<>0 then goto L4700 ! if displayed has an answer,but no match go back
04845 ! If PCDE<>0 Then Goto 14820  ! go back to read record in don't want selected invoices to show on grid  (not sure how to default)
04847 L4760: write #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
04849   if pcde=1 and displayunpaid<>2 then let total+=upa ! total paids
04851   if pcde=0 and displayunpaid=2 then let total+=upa ! total unpaids
04853   goto L4700
04855 DISPLAY_GRID: ! 
04857   mat chdr$(16) : mat cmask$(16) : mat flxitm$(16)
04859   let chdr$(1)="Rec" : let chdr$(2)="Bank" : let chdr$(3)="Pay"
04861   let chdr$(4)="Payee #" : let chdr$(5)="Invoice #"
04863   let chdr$(6)="Inv Date" : let chdr$(7)="Due Date"
04865   let chdr$(8)="Description": let chdr$(9)="Amount"
04867   let chdr$(10)="Dis Amt" : let chdr$(11)="Dis Date"
04869   let chdr$(12)="BK Code"
04871   let chdr$(13)="Ck #" : let chdr$(14)="D Paid"
04873   let chdr$(15)="P C" : let chdr$(16)="P Date"
04875   let cmask$(1)='30' : let cmask$(2)='30' : let cmask$(3)=""
04877   let cmask$(4)=''
04879   let cmask$(5)='' : let cmask$(6)='1': let cmask$(7)='1'
04881   let cmask$(8)='': let cmask$(9)='10' : let cmask$(10)='10'
04883   let cmask$(11)='3' : let cmask$(12)='30' : let cmask$(13)='30'
04885   let cmask$(14)='1' : let cmask$(15)='30' : let cmask$(16)="1"
04887 RE_DISPLAY_GRID: ! save a little time
04889   let fntos(sn$="paidinv")
04891   let respc=0 : mat resp$=('')
04893   let fnlbl(1,1,trim$(cnam$(1:30))&"-"&type$,65,2)
04895   let fnflexinit1('unpaidinv',5,27,15,55,mat chdr$,mat cmask$,1)
04897   restore #clearing: 
04899   if nextrec>0 and displayattop$="True" then goto L4890 else goto L5030
04901 L4890: for j=nextrec to lrec(clearing) ! read starting with next record
04903     read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L4940
04905     let flxitm$(9)=str$(upa) : let flxitm$(10)=str$(disamt)
04907     let flxitm$(11)=str$(ddate) : let flxitm$(3)=str$(pcde)
04909     let flxitm$(2)= flxitm$(12)=str$(bcde): let flxitm$(13)=str$(ckn)
04911     let flxitm$(14)=str$(dp) : let flxitm$(15)=str$(gde)
04913     let flxitm$(16)=str$(pdte)
04915     let flxitm$(1)=str$(rec(clearing))
04917     if pcde=1 then let flxitm$(3)="Yes" else if pcde=0 then let flxitm$(3)="No" else if pcde=1 and dp>0 then let flxitm$(3)="Paid"
04919 let fnflexadd1(mat flxitm$)
04940 L4940: next j
04950 if nextrec=1 then goto L5020 ! thinks it rereads the 1st record twice
04960 for j=1 to max(nextrec-1,1) ! read records previously coded or skipped
04970   read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
04980   let flxitm$(9)=str$(upa) : let flxitm$(10)=str$(disamt)
04990   let flxitm$(11)=str$(ddate) : let flxitm$(3)=str$(pcde)
05000   let flxitm$(2)= flxitm$(12)=str$(bcde): let flxitm$(13)=str$(ckn)
05010   let flxitm$(14)=str$(dp) : let flxitm$(15)=str$(gde)
05020   let flxitm$(1)=str$(rec(clearing))
05030   if pcde=1 then let flxitm$(3)="Yes" else if pcde=0 then let flxitm$(3)="No" else if pcde=1 and dp>0 then let flxitm$(3)="Paid"
05040 let fnflexadd1(mat flxitm$)
05050 next j
05055 L5020: goto L5070
05057 L5030: ! 
05059 read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
05061 let flxitm$(9)=str$(upa) : let flxitm$(10)=str$(disamt)
05063 let flxitm$(11)=str$(ddate) : let flxitm$(3)=str$(pcde)
05065 let flxitm$(2)= flxitm$(12)=str$(bcde): let flxitm$(13)=str$(ckn)
05067 let flxitm$(14)=str$(dp) : let flxitm$(15)=str$(gde)
05069 let flxitm$(16)=str$(pdte)
05071 let flxitm$(1)=str$(rec(clearing)) ! assign flxitm$(1) with new record #
05073 if pcde=1 then let flxitm$(3)="Yes" else if pcde=0 then let flxitm$(3)="No" else if pcde=1 and dp>0 then let flxitm$(3)="Paid"
05075 let fnflexadd1(mat flxitm$) : goto L5030
05077 L5070: let fnfra(2,1,13,23,"Approval Options"," ")
05079 let fnbutton(1,2,"&Approve All",62,"Will select to pay all unpaid invoices",1,18,1)
05081 let fnbutton(3,2,"&Approve by Range",63,"Enter a range of reference numbers to approve.  The reference # is the number to the left assigned by the computer.",1,18,1)
05083 let fnlbl(4,4,"From:",5,1,0,1)
05085 let fntxt(4,11,5,0,1,"30",0,"Select the first reference # to be approved",1)
05087 let resp$(respc+=1)=""
05089 let fnlbl(5,4,"To:",5,1,0,1)
05091 let fntxt(5,11,5,0,1,"30",0,"Select the last reference # to be approved",1)
05093 let resp$(respc+=1)=""
05095 let fnbutton(7,2,"&Approve by Due Date",64,"Approve all invoices due by a certain date.",1,18,1)
05097 let fnlbl(8,2,"Date:",5,1,0,1)
05099 let fntxt(8,8,8,0,1,"1",0,"All invoices with a due by date equal to or less than this date will be approved",1)
05101 let resp$(respc+=1)=""
05103 ! Let FNBUTTON(10,2,"Approve &Highlighted",65,"Approves one invoice at a time.  Highlight the selected invoice. To remove the approval, use this option and highlight the same invoice second time.",1,18,1,0,1)
05105 let fnbutton(10,2,"Approve By Payee",66,"Approves all invoices with this payee number in invoice record.",1,18,1)
05107 let fnlbl(11,2,"Payee #:",8,1,0,1)
05109 let fntxt(11,11,8,0,1,"",0,"Enter payee # to approve all invoices on that payee",1)
05111 let resp$(respc+=1)=""
05113 if displayunpaid=1 or displayunpaid=0 then let wording$="Total Selected:" else let wording$= "Total Unapproved:"
05115 let fnlbl(2,28,wording$,18,1)
05117 let fntxt(2,49,12,0,1,"10",0," ")
05119 let resp$(respc+=1)=str$(total)
05121 let fnchk(3,47,"Display at Top:",1)
05123 let resp$(respc+=1)=displayattop$
05125 let fncmdkey("&Approve Highlighted",1,1,0,"Approves or cancels the invoice that is highlighted.")
05127 let fncmdkey("&Display All",9,0,0,"Displays all remaining records in the unpaid file.")
05129 let fncmdkey("&Display Selected",3,0,0,"Displays all invoices selected for payment")
05131 let fncmdkey("&Display UnSelected",2,0,0,"Displays all remaining uncleared invoices")
05133 let fncmdkey("C&omplete",5,0,1,"Return to main unpaid invoice menu")
05135 let fnacs(sn$,0,mat resp$,ck)
05137 let displayunpaid=total= clear_from_range=displayall=0
05139 if ck=5 or ck=cancel then goto MENU1
05141 let selectedrec=val(resp$(1)) ! selected record from grid
05143 let rangefrom=val(resp$(2)) ! if select range of reference numbers
05145 let rangeto=val(resp$(3)) ! if select range of reference numbers
05147 let duedate =val(resp$(4)) ! used in selecting invoices by due date
05149 let payeevn$=resp$(5) ! payee number to select
05151 let total=val(resp$(6)) ! total used for display only
05153 let displayattop$=resp$(7) ! display at top
05155 if ck=2 then let displayunpaid=2: goto CODE_FOR_PAYMENT !                                                   redisplay on uncoded
05157 if ck=3 then let displayunpaid=1: goto CODE_FOR_PAYMENT ! displays only                                       cleared on this date
05159 if ck=9 then let displayall=1: goto CODE_FOR_PAYMENT ! displays everything                                 in unpaid file
05161 if ck=62 then goto PAY_ALL
05163 if ck=63 and rangefrom=0 then goto MSGBOX3
05165 if ck=69 then goto APPROVE_BY_RANGE
05167 if ck=64 and duedate=0 then goto MSGBOX4
05169 if ck=64 then goto CLEAR_BY_DUEDATE
05171 if ck=65 then goto APPROVE ! approve or unselect an invoice
05173 if ck=66 then goto APPROVE_BY_PAYEE ! approve all invoices for a                                                      specific payee
05175 APPROVE: ! clear or unclear selected invoices
05177 read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=selectedrec: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
05179 if pcde=0 then let pcde=1 : let newbcde=bankcode : goto L5540 ! if no previous payment code, use new one; if it has a payment code, change it
05181 if pcde=1 and dp=0 then let pcde=0 : let newbcde=0: goto L5540 ! change from yes to no
05183 if pcde=0 then let pcde=1 : let newbcde= bankcode: goto L5540 ! change from no to yes
05185 if pcde=1 and dp>0 then let pcde=1 : let newbcde=bcde: goto L5540 ! don't change previously paid
05540 L5540: ! 
05550 if pcde=1 then let flxitm$(3)="Yes" else if pcde=0 then let flxitm$(3)="No" else if pcde=1 and dp>0 then let flxitm$(3)="Paid"
05560 ! Print PCDE,BCDE
05570 rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=selectedrec: pcde,newbcde
05580 rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,newbcde ! update the transaction history
05590 let lastrec=selectedrec
05600 if lastrec+1 <= lrec(clearing) then let nextrec=lastrec+1 else let nextrec=1
05610 goto RE_DISPLAY_GRID
05620 goto CODE_FOR_PAYMENT
05630 MSGBOX3: ! need range of reference numbers
05640 mat ml$(2)
05650 let ml$(1)="You must enter the 'Range From' and 'Range To'"
05660 let ml$(2)="reference numbers to choose this option."
05670 let fnmsgbox(mat ml$,resp$,cap$,16)
05680 goto CODE_FOR_PAYMENT
05690 MSGBOX4: ! need due date for selecting by due date
05700 mat ml$(2)
05710 let ml$(1)="You must enter the 'Due Date' if you choose to'"
05720 let ml$(2)="approve by due date."
05730 let fnmsgbox(mat ml$,resp$,cap$,16)
05740 goto CODE_FOR_PAYMENT
05750 MSGBOX5: ! allocations don't match total invoice
05760 mat ml$(3)
05770 let ml$(1)="The allocations of "&trim$(cnvrt$("pic($$$$,$$$.##)",tac))&" do not agree with"
05780 let ml$(2)="the total invoice of "&trim$(cnvrt$("pic($$$$,$$$.##)",upa))&"."
05790 let ml$(3)="You must correct the problem before you can continue!"
05800 let fnmsgbox(mat ml$,resp$,cap$,16)
05810 goto ADD_UNPAID_INVOICES_2
05820 MSGBOX6: ! invoice number required
05830 mat ml$(3)
05840 let ml$(1)="You must enter an invoice number on each unpaid "
05850 let ml$(2)="record.  If you must make up an invoice #, be careful to use"
05860 let ml$(3)="a different number each time!"
05870 let fnmsgbox(mat ml$,resp$,cap$,16)
05880 return 
05890 PAY_ALL: ! pay all unpaid invoices
05900 restore #paytrans: 
05905 L5710: ! 
05910 read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5760
05915 if bcde=0 then let bcde=bankcode
05920 if pcde=0 then let pcde=1: goto L5740
05925 goto L5710
05927 L5740: ! 
05929 rewrite #paytrans,using 'Form POS 73,n 1,n 2': pcde,bcde ! update the transaction history
05931 goto L5710
05933 L5760: ! 
05935 goto CODE_FOR_PAYMENT
05937 APPROVE_BY_RANGE: ! clear by reference # range
05939 for j=rangefrom to rangeto
05941   read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5860 norec L5860
05943   if pcde>0 then goto L5850 ! already coded
05945   if pcde=0 then let pcde=1
05947   if bcde=0 then let bcde=bankcode ! don't change bank # if one                                                      previously entered
05949   rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,bcde ! update the transaction history
05951   rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=j: pcde,bcde ! update the transaction history
05953 L5850: ! 
05955 next j
05957 L5860: ! 
05959 goto CODE_FOR_PAYMENT
05961 CLEAR_BY_DUEDATE: ! clear any invoices with due date less than or equal the one entered
05963 for j=1 to lrec(clearing)
05965   read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5970 norec L5970
05967   if val(up$(2))<=duedate then goto L5910 else goto L5960
05969 L5910: ! 
05971   if pcde>0 then goto L5960 ! already coded
05973   if pcde=0 then let pcde=1
05975   if bcde=0 then let bcde=bankcode ! don't change bank # if one previously entered
05977   rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,bcde ! update the transaction history
05979   rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=j: pcde,bcde ! update the transaction history
05981 L5960: ! 
05983 next j
05985 L5970: ! 
05987 goto CODE_FOR_PAYMENT
05989 APPROVE_BY_PAYEE: ! select payee to pay
05991 restore #paytrans: 
06000 L6000: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L6060 : let lastrec=rec(paytrans)
06010 if uprc$(lpad$(rtrm$(payeevn$),8))<>uprc$(vn$) then goto L6000
06020 if pcde<>1 then let pcde=1
06030 if bcde=0 then let bcde=bankcode ! don't change bank # if one                                                      previously entered
06040 rewrite #paytrans,using 'Form POS 73,n 1,n 2',rec=lastrec: pcde,bcde ! update the transaction history
06050 goto L6000
06060 L6060: goto CODE_FOR_PAYMENT
06070 JOBCOST: ! 
06080 dim jn$*6,jname$*40
06090 ENTRY_SCREEN: ! 
06100 let fntos(sn$="Jobcost")
06110 let respc=0 : mat resp$=(''): let lc=0: let mylen=20: let mypos=mylen+3
06120 let fnlbl(lc+=1,1,"Payee # "&trim$(vn$)&" Invoice # "&trim$(iv$),50,0)
06130 let fnlbl(lc+=2,1,"Job #:",mylen,1)
06140 ! Let FNTXT(LC,MYPOS,6,0,1,"",0,"Choose from the sub-category list.")
06150 ! .  !  Let RESP$(RESPC+=1)=JN$
06160 let fncmbjob(lc,mypos)
06170 let resp$(respc+=1)=jn$
06180 let fnlbl(lc+=2,1,"Category #:",mylen,1)
06190 let fncmbcategory(lc,mypos)
06200 let resp$(respc+=1)=str$(cat)
06210 let fnlbl(lc+=2,1,"Sub-category #:",mylen,1)
06220 let fncmbsubcat(lc,mypos)
06230 let resp$(respc+=1)=str$(subcat)
06240 let fnlbl(lc+=1,1,"Amount:",mylen,1)
06250 let fntxt(lc,mypos,12,0,1,"10",0,"Enter the amount allocated to this category.")
06260 let resp$(respc+=1)=str$(amt)
06270 let fnlbl(lc+=1,1,"Description:",mylen,1)
06280 let fntxt(lc,mypos,25,0,0,"",0,"Enter the descritpion for the allocation.")
06290 let resp$(respc+=1)=jobdesc$
06300 ! Job Cost Invoice Breakdown Grid
06310 let fnflexinit1('JobAlloc',11,1,6,60,mat chdr3$,mat cmask3$,1,0,0)
06320 if displayalljobs=1 then restore #jcbreakdown: : goto L6270
06330 restore #jcbreakdown,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey EO_FLEX1
06335 L6270: let totalcost=0: mat jobitem$=("")
06340 READ_JOB_ALLOCATIONS: ! 
06345 read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,holdvn$,holdiv$ eof EO_FLEX1
06350 if displayalljobs=1 then goto L6320 ! allow all entries to print
06355 if holdvn$<>vn$ or holdiv$<>iv$ then goto EO_FLEX1
06357 L6320: ! 
06359 let totalcost+=amt
06361 let jobitem$(1)=str$(rec(jcbreakdown)) : let jobitem$(2)=jn$
06363 let jobitem$(3)=str$(cat) : let jobitem$(4)=str$(subcat)
06365 let jobitem$(5)=str$(amt) : let jobitem$(6)=jobdesc$
06367 let fnflexadd1(mat jobitem$)
06369 goto READ_JOB_ALLOCATIONS
06371 EO_FLEX1: ! 
06373 let fnbutton(3,70,"&Search",68,"Will search for job numbers",1,9)
06375 let fnbutton(5,70,"&Search",69,"Will search for available category codes",1,9)
06377 let fnlbl(18,18,"Total: "&trim$(cnvrt$("pic($$$,$$$,$$$.##)",totalcost)),22,right,0)
06379 if displayalljobs=0 then let fnlbl(19,18,"Invoice: "&trim$(cnvrt$("pic($$$,$$$,$$$.##)",upa)),22,right,0)
06381 let fnbutton(18,53,"&Edit",65,"Will allow you to change an allocation",1,5)
06383 let fnbutton(3,70,"&Search",63,"Will search for available category codes",1,9)
06385 let fncmdkey("&Next",1,1,0,"Accept this transaction)")
06387 let fncmdkey("&Listing",4,0,0,"Print listing of all job cost entries.")
06389 let fncmdkey("&Post To Jobs",3,0,0,"Post this batch ofjob cost entries to job cost records. Normally done once complete with batch.")
06391 let fncmdkey("&Cancel",5,0,1,"Cancels without posting to jub cost)")
06393 let fnacs(sn$,0,mat resp$,ck)
06395 if ck=4 then gosub PRINT_JOB_COST_ENTRIES: goto ENTRY_SCREEN
06397 if val(resp$(4))=0 and ck<>65 then let ck=5 ! exit if no amount on next
06399 if ck=5 then let amt=0: let totalcost=0 : goto L6930 ! Let SCREEN=0: Goto MENU1
06401 if ck=3 then gosub POST_TO_JOB : goto ENTRY_SCREEN
06403 if ck=65 then goto L6520 else goto L6530
06520 L6520: ! 
06530 let editrec=val(resp$(6))
06540 read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12",rec=editrec: jn$,cat,subcat,amt,jobdesc$,vn$,iv$
06550 delete #jcbreakdown,rec=editrec: norec ENTRY_SCREEN
06560 goto ENTRY_SCREEN
06565 L6530: if ck=68 then goto L6540 else goto L6550
06567 L6540: let jn$="": let fnjob_srch(jn$,1) : goto ENTRY_SCREEN
06569 L6550: let jn$=resp$(1)(1:6)
06571 let jn$=lpad$(rtrm$(jn$),6)
06573 read #41,using 'form pos 7,c 25',key=jn$: jobname$ nokey L6590
06575 goto L6600
06590 L6590: ! 
06600 mat ml$(3)=("")
06610 let ml$(1)="Job # "&jn$&" does not exist."
06620 let ml$(2)="                                        "
06630 let ml$(3)="Take OK to select a different job #."
06640 let fnmsgbox(mat ml$,resp$,cap$,0)
06650 goto ENTRY_SCREEN
06655 L6600: if ck=69 then goto L6610 else goto L6620
06657 L6610: let cn$="": let fncategory_srch(cn$,1) : let cat=val(cn$): goto ENTRY_SCREEN
06659 L6620: let cat=val(resp$(2)(1:5))
06661 let subcat=val(resp$(3)(1:3))
06663 let amt=val(resp$(4))
06665 let jobdesc$=resp$(5)
06667 write #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$
06669 let amt=0: goto ENTRY_SCREEN
06671 POST_TO_JOB: ! 
06673 restore #jcbreakdown: 
06700 L6700: read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$ eof L6900
06710 if ltrm$(jn$)="" or rtrm$(ltrm$(jn$))="0" then goto L6700
06720 let cn$=jn$&lpad$(str$(cat),5)
06730 read #2,using L6740,key=cn$: mat l,mat ta nokey L6780
06740 L6740: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
06750 let l(6)=l(6)+amt
06760 let l(9)=l(9)+amt
06770 goto L6780
06780 L6780: read #45,using L6790,rec=1,reserve: ot5
06790 L6790: form pos 86,pd 3
06800 let empnum$=lpad$(rtrm$(str$(ji1(1))),12)
06810 L6810: let ot5=lrec(45)+1
06820 let invdate=val(up$(1))
06830 write #45,using L6840,rec=ot5,reserve: "",jn$,cat,subcat,0,invdate,0,0,0,0,amt,jobdesc$,0 duprec L6810
06840 L6840: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
06850 if ta(2)=0 then let ta(1)=ot5 else rewrite #45,using L6790,rec=ta(2),reserve: ot5
06860 rewrite #45,using L6790,rec=1,release: ot5
06870 let ta(2)=ot5
06880 rewrite #2,using L6740,key=cn$: mat l,mat ta
06890 goto L6700
06900 L6900: let jn$="": let jobdesc$="": let amt=0: let cat=subcat=0
06910 close #jcbreakdown: 
06920 execute "Drop Q:\CLmstr\jcbreakdowns"&wsid$&".h"&str$(cno)
06930 L6930: return 
06940 L6940: open #jcbreakdown=46: "Name=Q:\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno)&",Version=1,replace,RecL=79",internal,outin,relative 
06950 close #jcbreakdown: 
06960 execute "INDEX Q:\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno)&",Q:\CLmstr\jcbrkidx"&wsid$&".H"&str$(cno)&",48,20,Replace,DupKeys -n"
06970 return 
06980 HDR2: ! header for jub cost listing
06990 let fnopenprn
07000 print #255,using 'Form POS 1,C 8,Cc 82': date$,cnam$
07010 print #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg,"Job Cost Entry Listing"
07020 print #255: ""
07030 print #255: " Payee #    Invoice #    Job #    Cat #  Sub-Cat   Amount  Descripton"
07040 print #255: " _______    _________    _____    _____  _______   ______  __________"
07050 return 
07060 ! ______________________________________________________________________
07070 PRINT_JOB_COST_ENTRIES: ! 
07080 let total_allocations=0
07090 gosub HDR2
07100 restore #jcbreakdown: 
07105 L7100: ! 
07110 read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$ eof L7140
07115 let total_allocations+=amt
07120 print #255,using "form pos 1,c 8,x 2,c 12,x 2,c 6,x 2,n 5,x 2,n 6,x 2,pic(zzz,zzz.##cr),c 30,skip 1": vn$,iv$,jn$,cat,subcat,amt,jobdesc$ pageoflow L7170
07125 goto L7100
07140 L7140: print #255,using "form pos 48,c 10,skip 1,pos 48,pic(zzz,zzz.zzcr),skip 1,pos 48,c 10": "__________",total_allocations,"=========="
07150 let fncloseprn
07160 let jn$="": let cat=0: let subcat=0: let amt=0: let jobdesc$=""
07170 return 
07175 L7170: print #255: newpage
07180 gosub HDR2
07185 continue 
07190 def fntest_key(holdkey$*20,vn$,iv$,cap$*128)
07195   dim newkey$*20
07200 ! uses open files:
07205   let newkey$=rpad$(vn$&iv$,20)
07210   if newkey$=holdkey$ then goto TEST_KEY_OK
07215 ! 
07220 TEST1: ! 
07225 ! pass goes to test2 - fail goes to test_key_fail_on_iv
07230   close #ivpaid: ioerr L320
07232 L320: open #ivpaid:=fngethandle: "Name=Q:\CLmstr\IvPaid.h"&str$(cno)&",KFName=Q:\CLmstr\IvIndex.h"&str$(cno),internal,outin,keyed 
07234   let unpaidkey$=rpad$(ltrm$(vn$),8)&rpad$(ltrm$(iv$),12)
07236   read #ivpaid,using 'Form Pos 1,C 8',key=unpaidkey$,release: x$ nokey TEST2
07238   goto TEST_KEY_FAIL_ON_IV
07240 ! ___________
07242 TEST2: ! 
07244 ! pass goes to test_key_pass - fail goes to test_key_fail_on_paytrans
07246   open #testpaytrans:=fngethandle: "Name=Q:\CLmstr\PayTrans.h"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",SHR",internal,outin,keyed 
07248   read #testpaytrans,using 'Form Pos 1,C 8',key=newkey$,release: x$ nokey TEST_KEY_OK
07250   goto TEST_KEY_FAIL_ON_PAYTRANS
07252 ! ____________
07254 TEST_KEY_FAIL_ON_PAYTRANS: ! 
07256   mat ml$(3)=("")
07258   let ml$(1)="The invoice number "&trim$(iv$)&" for Payee "&trim$(vn$)
07260   let ml$(2)="already exists in the Unpaid Invoice file."
07262   let ml$(3)="Please change the Invoice Number or the Payee."
07264   let fnmsgbox(mat ml$,resp$,cap$,0)
07266   goto TEST_KEY_FAIL
07268 ! ___________
07270 TEST_KEY_FAIL_ON_IV: ! 
07272   mat ml$(3)=("")
07274   let ml$(1)="The invoice number "&trim$(iv$)&" for Payee "&trim$(vn$)
07276   let ml$(2)="already exists in the Paid Invoice file."
07278   let ml$(3)="Please change the Invoice Number or the Payee."
07280   let fnmsgbox(mat ml$,resp$,cap$,0)
07282   goto TEST_KEY_FAIL
07284 ! ___________
07286 TEST_KEY_OK: ! 
07288 ! Print 'fnTest Key PASSED'
07290   let fntest_key=1
07292   goto EO_TEST_KEY
07294 ! ___________
07296 TEST_KEY_FAIL: ! 
07298 ! Print 'fnTest Key FAILED'
07300   let fntest_key=2
07302   goto EO_TEST_KEY
07304 ! ___________
07306 EO_TEST_KEY: ! 
07308 ! If FILE(IVPAID)<>0 Then Close #IVPAID:
07310   if file(testpaytrans)<>0 then close #testpaytrans: ioerr L630
07312 L630: ! 
07314 fnend 
