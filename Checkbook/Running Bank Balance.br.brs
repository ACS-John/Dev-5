00010 ! formerly S:\acsCL\BankBal
00020 ! Running Bank Balance
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fndat,fnerror,fntos,fnlbl,fntxt,fncmdset,fnacs, fndate_mmddyy_to_ccyymmdd,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,de$*35,bn$*30,ml$(0)*100
00080 ! ______________________________________________________________________
00100   fntop(program$)
00120   fndat(dat$,1)
00130   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative
00132   read #20,using 'Form POS 152,N 2',rec=1,release: bank_code 
00134   close #20: 
00140 MAIN: ! 
00150   fntos(sn$="bankbal")
00152   let respc=0
00160   fnlbl(1,40,"",1,1)
00170   fnlbl(1,1,"Starting Date:",31,1)
00180   fntxt(1,33,10,0,1,"3") 
00182   let resp$(respc+=1)=""
00190   fnlbl(2,1,"Beginning Checkbook Balance:",31,1)
00200   fntxt(2,33,12,0,1,"10")
00202   let resp$(respc+=1)=""
00210   fnlbl(3,1,"Bank Number to Print:",31,1)
00220   fntxt(3,33,2,0,1,"30") 
00222   let resp$(respc+=1)=str$(bank_code)
00230   fncmdset(2)
00232   fnacs(sn$,0,mat resp$,ckey)
00240   if ckey=5 then goto XIT
00250   let d1=val(resp$(1))
00260   b1=val(resp$(2))
00270   bank_code=val(resp$(3))
00280 ! ______________________________________________________________________
00290   open #20: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&env$('cno')&", KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&env$('cno')&",Shr", internal, outin, keyed  
00292   read #20,using 'Form POS 3,C 30',key=lpad$(str$(bank_code),2),release: bn$ nokey MAIN
00300   close #20: 
00310 ! ______________________________________________________________________
00320   close #trmstr: ioerr ignore
00330   execute "Index "&env$('Q')&"\CLmstr\TrMstr.H"&env$('cno')&' '&env$('Q')&"\CLmstr\Tridx3.H"&env$('cno')&" 16/12/4 2/4/8 Replace DupKeys -n" ! index in year,monthday,reference
00340 ! ______________________________________________________________________
00350   open #trmstr=5: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&env$('cno')&", KFName="&env$('Q')&"\CLmstr\Tridx3.H"&env$('cno')&",Shr", internal, outin, keyed ioerr ignore
00360   fnopenprn
00370   gosub HDR
00380   goto READ_1
00390 ! ______________________________________________________________________
00400 READ_1: ! 
00410   let key$=cnvrt$("pic(########)",d1)(3:8): let key$=key$&"        " ! Let KEY$=KEY$(3:6)&KEY$(1:2)&"        "
00420 ! Restore #TRMSTR,Key>=KEY$: Ioerr 440 ! need message box   (no dates in this range)
00421   restore #trmstr: ioerr L440
00430   goto READ_2
00440 L440: mat ml$(2) 
00442   let ml$(1)='There are no transactions for' 
00444   let ml$(2)="the date entered.  Check the date." 
00446   fnmsgbox(mat ml$) 
00448 goto MAIN
00450 READ_2: ! 
00460   read #trmstr,using 'Form POS 1,N 2,N 1,C 8,g 6,PD 10.2,POS 36,C 35': tbank_code,tcde,ck$,d2,amt,de$ eof ENDALL
00470   if fndate_mmddyy_to_ccyymmdd(d2)<d1 then goto READ_2
00480   if tbank_code<>bank_code then goto READ_2
00490   if tcde=2 or tcde=3 then let p1=68 else let p1=56
00500   if tcde=2 or tcde=3 then b1=b1+amt else b1=b1-amt
00510   pr #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 12.2,POS 80,N 12.2': ck$,d2,de$,amt,b1 pageoflow NEWPGE
00520 goto READ_2
00530 ! ______________________________________________________________________
00540 NEWPGE: pr #255: newpage: gosub HDR : continue 
00550 ! ______________________________________________________________________
00560 HDR: ! r:
00570   pr #255,using 'Form POS 1,C 8,Cc 76': date$,env$('cnam')
00580   pr #255,using 'Form POS 1,C 8,POS 36,C 40': time$,"Running Bank Balance"
00590   let pf2=46-int(len(rtrm$(bn$))/2)
00600   pr #255,using 'Form POS PF2,C 30': bn$
00610   pr #255,using 'Form POS 1,C 4,N 4,Cc 76': "Page",pg+=1,dat$ 
00612   pr #255: ""
00620   pr #255: "Ref-Numb    Date    Payee/Description                      Checks     Deposits    Balance "
00630   pr #255: "________  ________  ___________________________________  __________  __________  __________"
00640   if p1=0 then 
00642     let d1$=cnvrt$("pic(####/##/##)",d1) 
00644     let d3=val(d1$(6:7))*10000+val(d1$(9:10))*100+val(d1$(3:4)) 
00646     pr #255,using 'Form POS 1,C 10,pic(zz/zz/zz),X 2,C 35,POS 80,N 12.2': "",d3,"Beginning Balance",b1
00648   end if
00650 return ! /r
00670 ENDALL: ! r:
00680   fncloseprn
00690   close #trmstr: 
00700 goto XIT ! /r
00720 XIT: let fnxit
00740 ! <Updateable Region: ERTN>
00750 ERTN: let fnerror(program$,err,line,act$,"xit")
00760   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00770   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00780   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00790 ERTN_EXEC_ACT: execute act$ : goto ERTN
00800 ! /region
