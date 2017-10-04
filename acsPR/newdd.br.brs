00010 ! Replace S:\acsPR\newDD
00020 ! Direct Deposit file creator
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnwin3b,fnopenprn,fncloseprn,fndate_mmddyy_to_ccyymmdd,fnxit,fntop,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnchk,fncmdset,fnmsgbox,fnGetPayrollDates
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,ml$(3)*80,tcp(32),cp(32),tdc(10)
00080   dim path$*30 ! Path to Save File to
00090   dim email$*30 ! OR E-Mail Address to send file to
00100   dim ta(2) ! first and last trailer address
00110   ! em17  = Last Payroll Date (from first screen of employee record,  not departmental record)
00120   ! tdt4  = Last Payroll Date (from Departmental record)
00130   ! tcp(32) = Net Pay
00140   dim idn$*23,resp$(10)*30,bankname$*23 ! (23) Immediate Destination Name
00145   dim imo$*10
00150   dim ion$*23 ! (23) Immediate Origin Name
00160   dim em$(3)*30 ! (1)=Emp Name, (2)=Emp Addr, (3)=Emp CSZ
00170   dim bankaccount$*20,bankrouting$*20,federalrouting$*20,fedid$*12,bankname$*23
00180 ! ______________________________________________________________________
00190   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
00200 ! ______________________________________________________________________
00210   fntop(program$,cap$="Direct Deposits")
00240   cancel=5
00250   crlf$=chr$(13)&chr$(10)
00260   open #mstr=1: "Name="&env$('Q')&"\PRmstr\RPmstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00270   open #dd=30: "Name="&env$('Q')&"\PRmstr\DD.h"&env$('cno')&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&env$('cno')&",Shr,kps=1,kln=10,Use",internal,outin,keyed 
00280   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00290   open #ddinfo=31: "Name="&env$('Q')&"\PRmstr\DDInfo.h"&env$('cno')&",RecL=256,Use",internal,outin,relative 
00300   if lrec(31)=0 then write #31,using "form pos 1,c 30,c 20,c 20,c 20,c 12,c 23,c 12",rec=1,release: path$,bankaccount$,bankrouting$,federalrouting$,fedid$,bankname$,banksaccount$
00310   read #31,using "form pos 1,c 30,c 20,c 20,c 20,c 12,c 23,c 12",rec=1: path$,bankaccount$,bankrouting$,federalrouting$,fedid$,bankname$,banksaccount$
00320   close #ddinfo: 
00330   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1)
00350   let path$='C:\DirDep.txt'
00360   goto SCREEN1
00370 ! ______________________________________________________________________
00380 XIT: let fnxit
00390 ! ______________________________________________________________________
00400 ERTN: let fnerror(program$,err,line,act$,"XIT")
00410   if uprc$(act$)<>"PAUSE" then goto L440
00420   execute "list -"&str$(line) : pause : goto L440
00430   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." !:
        pr "" : pause 
00440 L440: execute act$ : goto ERTN
00450 ! ______________________________________________________________________
00460 SCREEN1: ! 
00470   let mypos=55
00480 ASK_INFO: ! 
00490   fntos(sn$="DD") !:
        let respc=0
00500   fnlbl(1,90,"",1,1) ! bigger screen
00510   fnlbl(2,1,"Payroll Date:",mypos,1)
00520   fntxt(2,mypos+3,10,0,1,"3",0,"For current payroll, always use the calculation date.  You can transfer older payrolls by using a previous payroll date.")
00530   let resp$(respc+=1)=str$(d1)
00540   fnlbl(3,1,"Path to Save File to:",mypos,1) !:
        fntxt(3,mypos+3,30,0,0,"70",0,"The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'") !:
        let resp$(respc+=1)=path$
00550   fnlbl(5,1,"Your Bank Account #:",mypos,1) !:
        fntxt(5,mypos+3,12,0,0,"",0,"The right hand set of numbers at the bottom of your checks.") !:
        let resp$(respc+=1)=bankaccount$
00560   fnlbl(6,1,"Routing Number of your Bank:",mypos,1) !:
        fntxt(6,mypos+3,12,0,0,"",0,"The middle set of numbers at the bottom of your checks.") !:
        let resp$(respc+=1)=bankrouting$
00570   fnlbl(7,1,"Routing Number of Federal Reserve Used by Your BAnk:",mypos,1) !:
        fntxt(7,mypos+3,10,0,0,"",0,"You will have to call your bank for this.  Some times it is build into their software and is not needed.") !:
        let resp$(respc+=1)=federalrouting$
00580   fnlbl(8,1,"Your Bank Name:",mypos,1) !:
        fntxt(8,mypos+3,23,0,0,"",0,"") !:
        let resp$(respc+=1)=bankname$
00590   fnlbl(9,1,"Federal ID Number:",mypos,1) !:
        fntxt(9,mypos+3,12,0,0,"",0,"The Federal ID number can be found on any payroll report.") !:
        let resp$(respc+=1)=fedid$
00610   fnchk(11,mypos,"Print a Report:",1) !:
        let resp$(respc+=1)="True"
00615   fnchk(13,mypos,"Is this a test file?",1) !:
        let resp$(respc+=1)="False"
00620   fncmdkey("&Next",1,1,0,"Creadt the direct deposit files." ) !:
        fncmdkey("E&xit",5,0,1,"Returns to menu")
00630   fnacs(sn$,0,mat resp$,ckey) ! ask employee #
00640   if ckey=5 then goto XIT
00650   let ppd=val(resp$(1))
00660   let path$=resp$(2)
00670   bankaccount$=odi$=resp$(3) ! bank account #
00680   bankrouting$=imo$=lpad$(resp$(4),10) ! your bank routing number
00690   bnkrtn=val(resp$(4)) ! your bank routing number in numeric
00700   let federalrouting$=imd$=resp$(5)
00710   bankname$=ion$=resp$(6)
00720   let fedid$=cid$=resp$(7) : cid$="1"&cid$
00740   if resp$(8)="True" then let report$="Y" else let report$="N"
00745   if resp$(9)="True" then let testfile=1 else let testfile=0
00750   open #ddinfo=31: "Name="&env$('Q')&"\PRmstr\DDInfo.h"&env$('cno')&",RecL=256,Use",internal,outin,relative 
00760   rewrite #ddinfo,using "form pos 1,c 30,c 20,c 20,c 20,c 12,c 23,c 12",rec=1,release: path$,bankaccount$,bankrouting$,federalrouting$,fedid$,bankname$,banksaccount$
00770   close #ddinfo: 
00780   open #ddout=22: "Name=DDout"&wsid$&".txt,RecL=96,EOL=CRLF,Replace",external,output 
00790   if report$="Y" then let fnopenprn else goto READ_DD
00800   gosub HDR ! pr header
00810   gosub HDR1 ! file header
00820 READ_DD: ! 
00830 L830: read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17": key$,dd$,rtn,acc,acn eof END1
00840   mat tcp=(0): mat ttdc=(0)
00850   if rtn=0 and acc=0 then goto L830
00860   if uprc$(dd$)<>"Y" then goto READ_DD !:
          ! Y means Yes Direct Deposit is active for this person
00870   let key$=lpad$(rtrm$(ltrm$(key$)),8) !:
        read #mstr,using 'Form pos 9,3*C 30,Pos 162,N 6,Pos 173',key=key$: mat em$,em17 nokey L830
00880   if fndate_mmddyy_to_ccyymmdd(em17)<d1 then goto READ_DD !:
          ! first screen emp data had a last PR Date lower than the Payroll !:
          ! Date specified in this program.
00890 READ_CHECKINFO: ! 
00900   checkkey$=key$&"         "
00910   restore #4,key>=checkkey$: nokey L830
00920 L920: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L970
00930   if heno<>val(key$) then goto L970
00940   if prd><d1 then goto L920
00950   mat tcp=tcp+cp : mat ttdc=ttdc+tdc
00960   goto L920
00970 L970: if sum(tcp)=0 then goto READ_DD ! no pay on this person for this payroll date
00980   gosub DETAIL1
00990   goto READ_DD
01000 ! ______________________________________________________________________
01010 HDR1: ! File Header Record _____________________________________________
01020   let pcde=01 ! Priority Code
01030 ! if env$('client')="Washington Parrish" then let imd$=" 061000146" ! Immediate Destination   (routing # for federal reserve bank they use)
01040 ! if env$('client')="West Rest Haven" then let imd$=" 111000038" ! Immediate Destination   (routing # for federal reserve bank they use) ! most likely wrong
01050   if env$('client')="Billings" then let imd$=" 081505964" ! Immediate Destination   (routing # for federal reserve bank they use)
01060 ! if env$('client')="Washington Parrish" then let imo$=" 065201611" ! Immediate Origin  (routing # for your bank)
01070 ! if env$('client')="West Rest Haven" then let imo$=" 111905159" ! Immediate Origin (contains the routing number for your bank)
01080   if env$('client')="Billings" then let imo$=" 000017738" ! Immediate Origin (contains the routing number for your bank)
01090   let fcd$=date$("YYMMDD") ! File Creation Date
01100   let fct$=time$(1:2)&time$(4:5) ! File Creation Time
01110   let fidm$="A" ! File ID Modifier
01120   let rsz$="094" ! Record Size
01130   bf$="10" ! Blocking Factor
01140   let fc$="1" ! Format Code
01150   let idn$="Federal Reserve Bank   " ! (23) Immediate Destination Name
01160 ! if env$('client')="Washington Parrish" then let ion$="Parrish National       " ! (23) Immediate Origin Name  (your bank name)
01170 ! if env$('client')="West Rest Haven" then let ion$="State National Bank    " ! (23) Immediate Origin Name
01180   if env$('client')="Billings" then let ion$="Bank of Billings       " ! (23) Immediate Origin Name
01190   let rc$="" ! Reference Code
01200   write #ddout,using 'Form POS 1,G 1,PIC(##),C 10,C 10,G 6,G 4,C 1,C 3,C 2,C 1,C 23,C 23,C 7,C 1,c 2': 1,pcde,imd$,imo$,fcd$,fct$,fidm$,rsz$,bf$,fc$,idn$,ion$,rc$,"0",crlf$
01210 ! Company/Batch Header Record __________________________________________
01220   scc=220 ! Service Class Code !:
        cdd$="" ! Company Discretionary Data !:
        ecc$="PPD" ! Standard Entry Class Code !:
        ced$="Payroll  " ! Company Entry Descriptive
01230 ! if env$('client')="West Rest Haven" then cid$="1741551549" ! Company Identification  ! instructions say it is a 1 digit identification code designator plus a nine digit identification number (assure a 1 means federal id code plus the federal id #)
01240 ! if env$('client')="Washington Parrish" then cid$="1726001461" ! Company Identification
01250   if env$('client')="Billings" then cid$="1430903099" ! Company Identification
01260   eed$=date$("YYMMDD") ! Effective Entry Date !:
        osc$="1" ! Originator Status Code !:
        bn=1 !  BN=Batch Number
01270 ! if env$('client')="Washington Parrish" then odi$="20428027" ! Origination DFI Identification  (your bank account number)
01280 ! if env$('client')="West Rest Haven" then odi$=" 1055003" ! Origination DFI Identification  (your bank account number)
01290   if env$('client')="Billings" then odi$=" 0040118" ! Origination DFI Identification  (your bank account number)
01300   write #ddout,using 'Form POS 1,G 1,PIC(###),C 16,C 20,C 10,C 3,C 10,PIC(######),G 6,G 3,G 1,C 8,PIC(#######),c 2': 5,scc,env$('cnam')(1:16),cdd$="Payroll",cid$,ecc$,ced$,fncd(d2),eed$,"",osc$,odi$,bn,crlf$
01310   return 
01320 ! ______________________________________________________________________
01330 DETAIL1: ! r: entry detail
01340   let t1=t1+tcp(32)
01350   if acc=27 then let tc=22 else !:
          if acc=37 then let tc=32 !:
            ! BC ! Transaction Code used to be was TC=23
01360   ari=0 ! Addenda Record Indicator
01370   let tn1=tn1+1
01380   let tn$=cnvrt$("PIC(#######)",tn1) ! Trace Number
01390   let dr$="081505731" !:
        let da$="10004147         "
01395   if testfile=1 then let tcp(32)=0
01400   write #ddout,using 'Form POS 1,G 1,G 2,pic(########),C 1,C 17,PIC(##########),C 15,C 22,G 2,N 1,C 8,c 7,c 2': 6,tc,int(rtn/10),str$(rtn)(len(str$(rtn)):len(str$(rtn))),str$(acn),tcp(32)*100,z$,em$(1)(1:22),"",ari,lpad$(trim$(odi$),8),tn$,crlf$ !:
        ! changed dr$ to str(rtn) ; also da$ to str$(acn)  ! entry to place money in employees account
01420   if report$="Y" then !:
          pr #255: z$&" "&em$(1)&" "&str$(tcp(32)) pageoflow PRINT_NEWPAGE
01430   let td1=td1+(tcp(32)*100)
01440   let tc1+=(tcp(32)*100) ! added this for the batch totals - ??
01450   ! if env$('client')="West Rest Haven" and rtn=1190515 then let totalin=totalin+tcp(32)
01460   eh=eh+int(rtn/10) !:
        ! Entry Hash should accumulate Routing numbers !:
        ! dropping the last digit of the routing number
01470 return ! /r
01480 ! ______________________________________________________________________
01490 CTRL1: ! r: Company/Batch Control Record
01500   scc=220 ! Service Class Code
01510   eac=tn1 ! Entry Addenda Count
01520   ! eH=0 ! Entry Hash
01530   eh$=str$(eh): let x=len(eh$): eh=val(eh$(max(1,x-9):x))
01540   ! TD1=Total Debit Amount
01550   ! TC1=Total Credit Amount
01560   ! CID$=Company Identification
01570   if eh=0 then mat ml$(3) !:
          let ml$(1)="It appears you do not have anyone with" !:
          let ml$(2)="direct deposit this pay period." !:
          let ml$(3)="Click OK to continue." !:
          fnmsgbox(mat ml$,resp$,cap$,0) !:
          goto XIT
01580   write #ddout,using 'Form POS 1,G 1,PIC(###),PIC(######),PIC(##########),2*PIC(############),C 10,C 19,C 6,C 8,PIC(#######),c 2': 8,scc,eac,eh,td1,tc1,cid$,mac$,"",odi$,bn,crlf$ ! removed *100 from TD1 and from TC1
01590   ! 
01600   ! File Control Record
01610   bactr=1 ! Batch Count
01620   let tn2=tn1+4 !:
        ! total # Records (all 6 Records plus the 1&5 plus 8&9)
01630   if fp(tn2/10)>0 then !:
          blctr=int(tn2/10)+1: bkfactor=blctr*10-tn2 !:
          ! block counter and block factor
01640   if fp(tn2/10)=0 then blctr=int(tn2/10): bkfactor=0
01650   eac=tn1 ! entry/adgenda count (number of 6 Records)
01660   ! don't change my entry hash any more ! eH=0008150573 ! EH=Entry Hash
01670   if eh=0 then mat ml$(2) !:
          let ml$(1)="It appears you do not have anyone with" !:
          let ml$(2)="direct deposit this pay period." !:
          let ml$(3)="Click OK to continue." !:
          fnmsgbox(mat ml$,resp$,cap$,0) !:
          goto XIT
01675   write #ddout,using 'Form POS 1,G 1,G 2,pic(########),C 1,C 17,PIC(##########),C 15,C 22,G 2,N 1,C 8,c 7,c 2': 6,27,int(bnkrtn/10),str$(bnkrtn)(len(str$(bnkrtn)):len(str$(bnkrtn))),bankaccount$,td1,"","","",ari,lpad$(trim$(odi$),8),tn$,crlf$ !:
        ! changed dr$ to str(rtn) ; also da$ to str$(acn)  ! total entry for  debiting customer account
01680   write #ddout,using 'Form POS 1,G 1,2*PIC(######),PIC(########),PIC(##########),2*PIC(############),C 38,C 1,c 2': 9,bactr,blctr,eac,eh,td1,tc1,rpt$(" ",38)," ",crlf$ !:
        ! removed *100 from TD1 and TC1
01690   if bkfactor<>0 then !:
          for j=1 to bkfactor !:
            ! pr "l22="&STR$(L22+=1) !:
            write #ddout,using 'Form POS 1,C 96': rpt$("9",94)&crlf$ !:
          next j
01700   return ! /r
01710 ! ______________________________________________________________________
01720 END1: ! r:
01730   ! pr 'end1' ! XXX
01740   gosub CTRL1
01750   gosub ADD_LF
01760   if report$="Y" then pr #255,using L1770: "Total",tc1/100
01770   L1770: form pos 22,c 15,n 12.2,skip 1
01780   ! if report$="Y" and env$('client')="West Rest Haven" then pr #255,using L1770: "Total In House",totalin
01790   if report$="Y" then let fncloseprn
01800   goto XIT ! /r
01810 PRINT_NEWPAGE: ! r:
01820   pr #255: newpage
01830   gosub HDR
01840 continue ! /r
01850 HDR: ! r:
01860   ! pr #255,Using 1380: DATE$,,TIME$,CAP$
01870   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
01880   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
01890   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
01900   pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
01910   pr #255: "\ql   "
01920 return ! /r
01930 ! ______________________________________________________________________
01940 ADD_LF: ! r:
01950   close #ddout: 
01960 goto DONE_X ! /r
01970 DONE_X: ! r:
01980   if trim$(path$)<>'' and exists(path$) then execute "free "&path$&" -n"
01990   if trim$(path$)<>"" then execute "Copy DDout"&wsid$&".txt "&path$&" -n"
02000   if trim$(email$)<>"" then execute "sy Start Mailto:"&trim$(email$)&"?attach=DDout"&wsid$&".txt?subject=Direct_Deposit_Payroll" !:
          pr newpage !:
          pr f "10,1,Cc 80,N": "After sending your e-mail," !:
          pr f "11,1,Cc 80,N": "Pess ENTER to continue." !:
          input fields "1,1,C 1,N": pause$
02010   execute "Free DDout"&wsid$&".txt -n"
02020 return ! /r
02030 ! ______________________________________________________________________
