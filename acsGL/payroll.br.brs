00010 ! Replace S:\acsGL\Payroll
00020 ! GL Payroll File Menu
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fnchain,fntos,fnlbl,fncombof,fncmdkey,fnacs,fnemployee_search,fntxt,fncmdset,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl2$(5),sc2$(5)*38
00080   dim sc1$(26),fl1$(26),io1$(41),hd$(2)*78,pr1(21),miscname$(10)*20
00090   dim l1(4),l2$(4)*25,l3$(4)*25,sc3$(21)*13,fl3$(21),io3$(21),prd(21)
00100   dim l4$(4)*25,l5$(4)*11,mp(4,36),dat$*20,cnam$*40,pr$(21),td$*25
00110   dim k$(3)*25,ss$*11,m(36),adr(2),d(14),ext(2),en$*4,ta(2)
00120   dim sr$(16),en$(16),resp$(50)*35,ml$(4)*80
00130   dim iocr$(3),wrdcr$(3)*23
00140   dim p$(20)*50,cap$*128,dedcode(10)
00150 ! ______________________________________________________________________
00160   fntop(program$,cap$="Payroll")
00170   fncno(cno,cnam$)
00180   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #1,using 'Form POS 418,10*C 20,10*N 1': mat miscname$,mat dedcode !:
        close #1: 
00190 ! 
00200   sc1$(1)="Description:" !:
        sc1$(2)="  Y.T.D." !:
        sc1$(3)="  Q.T.D." !:
        sc1$(4)="Employee #:" !:
        sc1$(5)="Name F/M/L:"
00210   sc1$(6)="Address:" !:
        sc1$(7)="City St Zip:" !:
        sc1$(8)="Soc-Sec-#:" !:
        sc1$(9)="Gross Wage:" !:
        sc1$(10)="Fed W/H:"
00220   sc1$(11)="FICA W/H:" !:
        sc1$(12)="State W/H:" !:
        sc1$(13)="Local W/H:"
00230   for j=1 to 10: sc1$(j+13)=rtrm$(miscname$(j)(1:12))&":" : next j
00240   sc1$(24)="Tips:" !:
        sc1$(25)="Weeks Worked:" !:
        sc1$(26)="EIC:"
00250   sc3$(1)="Check Date:" !:
        sc3$(2)="Check #:"
00260   sc3$(21)="Net Pay:"
00270   if exists (env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)) =0 then goto INITIAL_BUILD
00280   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2900
00290   open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",Shr",internal,outin,relative 
00300   goto MAIN
00310 ! ______________________________________________________________________
00320 DONE: ! 
00330   close #1: 
00340   close #2: 
00350   if new1=1 or cont=1 then goto L1410
00360   goto XIT
00370 ! ______________________________________________________________________
00380 MAIN: ! 
00390   fntos(sn$="Payroll") !:
        let mylen=20: let mypos=mylen+3 : let right=1
00400   fnemployee_search(x$,99)
00410 ! Let FNLBL(1,1,"Employee Number:",MYLEN,RIGHT)
00420 ! Let FNCOMBOF("PRmstr",1,MYPOS,27,env$('Q')&"\GLmstr\PRmstr.h"&str$(cno),1,4,5,30,'',0,PAS, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0) !:
        let resp$(1)=str$(eno)
00430   fncmdkey("&Next",1,1,0,"")
00440   fncmdkey("&Add",2,0,0,"")
00450   fncmdkey("&Proof Llist",4,0,0,"")
00460   fncmdkey("&Cancel",5,0,1,"")
00470   fnacs(sn$,0,mat resp$,ckey)
00480 L480: if ckey=5 then goto XIT
00490   if ckey=2 then goto ASK_NEW_NUMBER
00500   if ckey=4 then goto PROOF_LIST
00510   let holden1=en1=val(resp$(1)(1:4))
00520   goto DISPLAY_RECORD
00530 ASK_NEW_NUMBER: !  add new employee
00540   eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0)
00550   fntos(sn$="Payroll2") !:
        let mylen=20: let mypos=mylen+3 : let right=1
00560   fnlbl(1,1,"Employee Number:",mylen,right)
00570   fntxt(1,mypos,4,0,0,"30",0,"Enter new employee number.",0 ) !:
        let resp$(1)=""
00580   fncmdkey("&Next",1,1,0,"")
00590   fncmdkey("&Cancel",5,0,1,"")
00600   fnacs(sn$,0,mat resp$,ckey)
00610   let holden1=en1=val(resp$(1))
00620   if en1=0 then goto MAIN
00630   addemployee=1 ! code for adding new employee
00640 DISPLAY_RECORD: ! 
00650   en$=lpad$(str$(en1),4)
00660   read #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5',key=en$: eno,mat k$,ss$,mat m,mat ta nokey DISPLAY_EMPLOYEE
00670   let disable=1: goto DISPLAY_EMPLOYEE
00680 DISPLAY_EMPLOYEE: ! 
00690   fntos(sn$="Payroll3") !:
        let mylen=15: let mypos=mylen+3 : let right=1
00700   fnlbl(1,1,"Employee Number:",mylen,right)
00710   fntxt(1,mypos,4,0,0,"30",disable,"",0 ) !:
        let resp$(1)=str$(en1)
00720   fnlbl(2,1,"Employee Name:",mylen,right)
00730   fntxt(2,mypos,25,0,0,"",0,"",0 ) !:
        let resp$(2)=k$(1)
00740   fnlbl(3,1,"Address:",mylen,right)
00750   fntxt(3,mypos,25,0,0,"",0,"",0 ) !:
        let resp$(3)=k$(2)
00760   fnlbl(4,1,"City, St Zip:",mylen,right)
00770   fntxt(4,mypos,25,0,0,"",0,"",0 ) !:
        let resp$(4)=k$(3)
00780   fnlbl(5,1,"Social Security:",mylen,right)
00790   fntxt(5,mypos,11,0,0,"",0,"",0 ) !:
        let resp$(5)=ss$
00800   let mylen2=20 : let mypos=mylen+50
00810   fnlbl(1,70,"Y T D         Q T D ",24,0)
00820   for j=1 to 18
00830     fnlbl(j+1,42,sc1$(j+8),mylen2,right)
00840     fntxt(j+1,mypos,12,0,0,"10",0,"",0 ) !:
          let resp$(j*2-1+5)=str$(m(j*2-1) )
00850     fntxt(j+1,mypos+14,12,0,0,"10",0,"",0 ) !:
          let resp$(j*2+5)=str$(m(j*2) )
00860   next j
00870   fncmdkey("&Next",1,1,0,"")
00880   fncmdkey("&Review Checks",3,0,0,"")
00885   fncmdkey("&Add Check",8,0,0,"")
00890   fncmdkey("&Change Number",7,0,0,"")
00900   fncmdkey("&Delete",6,0,0,"")
00910   fncmdkey("&Cancel",5,0,1,"")
00920   fnacs(sn$,0,mat resp$,ckey)
00930   let disable=1
00940   if ckey=5 then goto MAIN
00950   if ckey=6 then goto DELETEIT
00960   if ckey=3 then add=0: goto REVIEW_CHECKS
00970   if ckey=7 then let disable=0: goto DISPLAY_EMPLOYEE
00980   en1=eno=val(resp$(1))
00985   if ckey=8 then add=1: mat prd=(0): goto L2340
00990   let k$(1)=resp$(2)
01000   let k$(2)=resp$(3)
01010   let k$(3)=resp$(4)
01020   ss$=resp$(5)
01030   for j=1 to 36
01040     let m(j)=val(resp$(j+5))
01050   next j
01060   if ckey=1 and holden1<>en1 then goto MSGBOX2
01070   goto L1250
01080 DELETEIT: ! 
01090 MSGBOX1: ! 
01100   mat ml$(3) !:
        let ml$(1)="You have chosen to delete employee " !:
        let ml$(2)="number "&str$(eno)&".  Click OK to delete" !:
        let ml$(3)="this record or Cancel to retain the record." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
01110   if resp$="OK" then goto L1150 else goto MAIN
01120 MSGBOX2: ! 
01130   mat ml$(3) !:
        let ml$(1)="You are attempting to change the employee" !:
        let ml$(2)="number from "&str$(holden1)&" to "&str$(eno)&".  Click OK to change" !:
        let ml$(3)="the number or Cancel to retain the old number." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
01140   if resp$="OK" then goto L1170 else goto MAIN
01150 L1150: delete #1,key=lpad$(str$(en1),4): nokey MAIN
01160 ! delete or change numbers
01170 L1170: adr=ta(1)
01180 L1180: if adr=0 then goto L1240
01190   read #2,using L1200,rec=adr: en1,nta
01200 L1200: form pos 1,n 4,pos 108,pd 3
01210   if ckey=6 then delete #2,rec=adr: else rewrite #2,using L1200,rec=adr: eno,nta
01220   adr=nta
01230   goto L1180
01240 L1240: if ckey=6 then eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0) !:
          goto MAIN
01250 L1250: if ckey=1 and addemployee=1 then mat ta=(0): write #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m,mat ta: addemployee=0 else goto L1280
01260   let new1=1
01270   eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0): goto MAIN
01280 L1280: if ckey=1 then rewrite #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5',key=en$: eno,mat k$,ss$,mat m,mat ta nokey L1290
01290 L1290: eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0): goto MAIN
01300 ! ______________________________________________________________________
01310 INITIAL_BUILD: ! 
01320   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno),internal,output ioerr L1330
01330 L1330: close #1,free: ioerr L1340
01340 L1340: open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",SIZE=0,RecL=280,Replace",internal,output 
01350   close #2: ioerr L1360
01360 L1360: execute "Free "&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno) ioerr L1370
01370 L1370: open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno),internal,output ioerr L1380
01380 L1380: close #2,free: ioerr L1390
01390 L1390: open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",SIZE=0,RecL=110,Replace",internal,output,relative 
01400   close #2: 
01410 L1410: close #1: ioerr L1420
01420 L1420: execute "Index "&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&' '&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&" 1 4 Replace DupKeys -n"
01430   goto MAIN
01440 ! ______________________________________________________________________
01450 PROOF_LIST: ! 
01460   restore #1,key>="    ": eof L1470, nokey L1470
01470 L1470: let fnopenprn
01480   gosub HDR
01490 L1490: read #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m eof L1610
01500   let pl=pl+1
01510   l1(pl)=eno
01520   l2$(pl)=k$(1)
01530   l3$(pl)=k$(2)
01540   l4$(pl)=k$(3)
01550   l5$(pl)=ss$
01560   for j1=1 to 36
01570     let mp(pl,j1)=m(j1)
01580   next j1
01590   if pl=4 then gosub L1660
01600   goto L1490
01610 L1610: if pl>0 then gosub L1660
01620   on fkey 5 ignore 
01630   fncloseprn
01640   if fnprocess=1 then goto XIT else goto MAIN
01650 ! ______________________________________________________________________
01660 L1660: pr #255,using L1670: sc1$(4),mat l1
01670 L1670: form pos 1,c 21,x 7,pic(zzzz),x 24,pic(zzzz),x 24,pic(zzzz),x 24,pic(zzzz),skip 1
01680   pr #255,using L1690: sc1$(5),mat l2$
01690 L1690: form pos 1,c 21,3*c 28,c 25,skip 1
01700   pr #255,using L1690: sc1$(6),mat l3$
01710   pr #255,using L1690: sc1$(7),mat l4$
01720   pr #255,using L1690: sc1$(8),mat l5$
01730   for j1=1 to 36
01740     let j2=int((j1-1)/2)+9
01750     if fp(j1/2)=0 then sc1$=sc1$(j2)&"QTD" else sc1$=sc1$(j2)&"YTD"
01760     pr #255,using L1770: sc1$,mp(1,j1),mp(2,j1),mp(3,j1),mp(4,j1)
01770 L1770: form pos 1,c 21,pic(---------.##),pic(-------------------------.##),pic(-------------------------.##),pic(------------------------.##),skip 1
01780   next j1
01790   mat l1=(0)
01800   mat l2$=("")
01810   mat l3$=("")
01820   mat l4$=("")
01830   mat l5$=("")
01840   mat mp=(0)
01850   let pl1=pl1+1
01860   if pl1=2 then goto L1900
01870   pr #255,using L1880: " "
01880 L1880: form c 1,skip 4
01890   goto L1940
01900 L1900: let pl1=0
01910   pr #255: newpage
01920   if pl><4 then goto L1940
01930   gosub HDR
01940 L1940: let pl=0
01950   return 
01960 ! ______________________________________________________________________
01970 HDR: ! 
01980   pr #255,using L1990: date$('mm/dd/yy'),cnam$
01990 L1990: form skip 2,pos 1,c 8,pos 1,cc 108,skip 1
02000   pr #255,using L2010: time$,"Payroll Proof List",dat$
02010 L2010: form pos 1,c 8,pos 45,c 20,skip 1,pos 1,cc 108,skip 2
02020   return 
02030 ! ______________________________________________________________________
02040   pr newpage
02050   pr f "10,15,Cc 43,N": "Reassigning Transaction Addresses..."
02060   restore #1,key>="    ": eof L2070
02070 L2070: read #1,using 'Form POS 271,2*N 5': mat ta eof L2100
02080   rewrite #1,using 'Form POS 271,2*N 5': 0,0
02090   goto L2070
02100 L2100: lr2=lrec(2)
02110 ! REWRITE #2,USING 2360,REC=1: LR2
02120   for j=1 to lr2
02130     read #2,using 'Form POS 1,C 4,POS 108,PD 3',rec=j: en$,nta norec L2210
02140     read #1,using 'Form POS 271,2*N 5',key=en$: mat ta nokey L2210
02150     if ta(1)=0 then let ta(1)=j
02160     if ta(2)>0 then rewrite #2,using L2200,rec=ta(2): j
02170     let ta(2)=j
02180     rewrite #1,using 'Form POS 271,2*N 5',key=en$: mat ta
02190     rewrite #2,using L2200,rec=j: 0
02200 L2200: form pos 108,pd 3
02210 L2210: next j
02220   goto MAIN
02230 ! ______________________________________________________________________
02240 REVIEW_CHECKS: ! 
02250   if ta(1)=0 then goto MSGBOX5 else goto L2290
02260 MSGBOX5: ! 
02270   mat ml$(3) !:
        let ml$(1)="There are no checks on employee # "&str$(eno)&"." !:
        let ml$(2)="Do you wish to add checks?" !:
        fnmsgbox(mat ml$,resp$,cap$,35)
02280   if resp$="Yes" then add=1: goto L2340 else goto MAIN
02290 L2290: adr=ta(1)
02300 L2300: if adr=0 then goto MAIN
02310   read #2,using L2320,rec=adr: en2,mat prd,nca norec L480
02320 L2320: form pos 1,n 4,2*pd 4,19*pd 5.2,pd 3
02330 L2330: mat pr1=prd
02340 L2340: let fntos(sn$="Payroll4") !:
        let mylen=15: let mypos=mylen+3 : let right=1
02350   fnlbl(1,1,"Check Date:",mylen,right)
02360   fntxt(1,mypos,1,0,0,"1",0,"Date of check.",0 ) !:
        let resp$(1)=str$(prd(1))
02370   fnlbl(2,1,"Check Number:",mylen,right)
02380   fntxt(2,mypos,8,0,0,"30",0,"",0 ) !:
        let resp$(2)=str$(prd(2))
02390   fnlbl(3,1,"Gross Wage:",mylen,right)
02400   fntxt(3,mypos,12,0,0,"10",0,"",0 ) !:
        let resp$(3)=str$(prd(3))
02410   for j=1 to 17
02420     fnlbl(j+3,1,sc1$(j+9),mylen,right)
02430     fntxt(j+3,mypos,12,0,0,"10",0,"",0 ) !:
          let resp$(j+3)=str$(prd(j+3))
02440   next j
02450   fnlbl(21,1,"Net Pay:",mylen,right)
02460   fntxt(21,mypos,12,0,0,"10",0,"",0 ) !:
        let resp$(21)=str$(prd(21))
02470   fncmdkey("&Next",1,1,0,"")
02490   fncmdkey("&Cancel",5,0,1,"")
02500   fnacs(sn$,0,mat resp$,ckey)
02510   if ckey=5 then add=0: goto MAIN
02530   for j=1 to 21
02540     let prd(j)=val(resp$(j))
02550   next j
02560   let wh=0
02570   for j=3 to 21
02580     if j=3 then goto L2650 ! gross
02590     if j<8 then let wh=wh+prd(j) ! fed,fica,state,local
02600     if j>7 and j<18 and dedcode(j-7)=2 then let wh=wh-prd(j)
02610     if j>7 and j<18 and dedcode(j-7)<>2 then let wh=wh+prd(j)
02620     if j=18 then let wh=wh+prd(18) ! tips
02630     if j=19 then goto L2650 ! weeks worked
02640     if j=20 then let wh=wh-prd(j) ! eic
02650 L2650: next j
02660   if prd(3)<>prd(21)+wh then goto MSGBOX4 else goto L2700
02670 MSGBOX4: ! 
02680   mat ml$(4) !:
        let ml$(1)="Gross pay ("&trim$(cnvrt$("pic(zzzz,zzz.##)",prd(3)))&") less withholding " !:
        let ml$(2)="("&trim$(cnvrt$("pic(zzzz,zzz.##)",wh))&") does not equal" !:
        let ml$(3)="the net check ("&trim$(cnvrt$("pic(zzzz,zzz.##)",prd(21)))&")" !:
        let ml$(4)="Click OK to fix the check." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
02690   if resp$="OK" then goto L2340 else goto MAIN
02700 L2700: lr2=lrec(2)+1
02710   if add=1 then write #2,using L2320,rec=lr2: eno,mat prd,0 duprec L2700 else rewrite #2,using L2320,rec=adr: eno,mat prd,nca
02720   if add=0 then goto L2770
02730   if ta(2)>0 then rewrite #2,using L2740,rec=ta(2): lr2
02740 L2740: form pos 108,pd 3
02750   if ta(1)=0 then let ta(1)=lr2
02760   let ta(2)=lr2
02770 L2770: if add=0 then mat prd=prd-pr1 ! file maintenance
02780   for j=3 to 20
02790     if j=3 then let m1=1
02800     if j=4 then let m1=3
02810     if j=5 then let m1=5
02820     if j>5 then let m1=(j-5)*2+5
02830     let m(m1)=m(m1)+prd(j)
02840     let m(m1+1)=m(m1+1)+prd(j)
02850   next j
02860   rewrite #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5',key=en$: eno,mat k$,ss$,mat m,mat ta
02870   adr=nca
02875   if add=1 then mat pr1=(0): mat prd=(0): goto L2330
02880   if add=0 then goto L2300
02890 ! ______________________________________________________________________
02900 L2900: if err=4152 then goto INITIAL_BUILD else goto ERTN
02910 XIT: let fnxit
02920 ! ______________________________________________________________________
02930 ! ______________________________________________________________________
02940 ! <Updateable Region: ERTN>
02950 ERTN: let fnerror(program$,err,line,act$,"xit")
02960   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02970   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02980   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02990 ERTN_EXEC_ACT: execute act$ : goto ERTN
03000 ! /region
