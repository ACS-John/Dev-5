00010 ! Replace S:\acsCL\Label
00020 ! pr labels for payees
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnaddlabel,fnlabel,fnTos,fnLbl,fncomboa,fnChk,fnCmdSet,fnAcs,fnTxt,fncombof
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,cap$*128,io2$(3),wrd2$(3)
00080   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,prtbegin$*5
00090   dim labeltext$(5)*120,pt$(5),message$*40,item1$(3),resp$(10)*30
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$)
00120   fntop(program$,cap$="Payee Labels")
00130   cancel=99 : right=1 : limit_to_list=1 : on=1 !:
        off=0 : left=0 : center=2
00140 ! ______________________________________________________________________
00150 MAIN: ! 
00160   fnTos(sn$="cllabel-1")
00170   respc=0 : mylen=25 : mypos=mylen+2
00180   fnLbl(1,1,"Print Labels For:",mylen,right)
00190   fi$="cllabels" !:
        item1$(print_all=1)="[All]" !:
        item1$(check_range=2)="Range of Checks" !:
        item1$(specific_payees=3)="Specific payees" !:
        fncomboa(fi$,1,mypos,mat item1$,"The labels can be printed in Customer Number order,Customer Name order, or in Bar Code sequence") !:
        resp$(respc+=1)=item1$(1)
00200   fnChk(2,mypos+2,'Print Payee Number on Label',right) !:
        resp$(respc+=1)='False'
00210   fnLbl(4,1,"Bank:",mylen,right)
00220   fncombof('Bank',4,mypos,33,env$('Q')&"\CLmstr\BankMstr.h"&env$('cno'),1,2,3,30,env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno'),limit_to_list) !:
        resp$(respc+=1)=""
00230   fnLbl(5,1,"Starting Check Number:",25,1)
00240   fncombof('Check',5,mypos,33,env$('Q')&"\CLmstr\TrMstr.h"&env$('cno'),4,8,36,35) !:
        resp$(respc+=1)=""
00250   fnLbl(6,1,"Ending Check Number:",25,1)
00260   fncombof('Check',6,mypos,33,env$('Q')&"\CLmstr\TrMstr.h"&env$('cno'),4,8,36,35) !:
        resp$(respc+=1)=""
00270   fnLbl(8,1,"Starting Payee Number:",25,1)
00280 ! fnTxt(8,27,8,0,1,"",0,'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"')! !:
        ! rESP$(RESPC+=1)=""
00290   fncombof("Payee",8,27,20,env$('Q')&"\CLmstr\Paymstr.h"&env$('cno'),1,8,9,20,env$('Q')&"\CLmstr\Payidx1.h"&env$('cno'),1,0, "Select starting payee record for printing") !:
        resp$(respc+=1)=""
00300   fnCmdSet(2)
00310   fnAcs(sn$,0,mat resp$,ck) !:
        if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then prtall=print_all else !:
            if resp$(1)=item1$(2) then prtall=check_range else !:
              if resp$(1)=item1$(3) then prtall=specific_payees
00320   printpayeenum$=resp$(2) !:
        wbc=val(resp$(3)(1:2)) ! working bank code !:
        c1=val(resp$(4)(1:8)) ! starting check number !:
        c2=val(resp$(5)(1:8)) ! ending check number !:
        vn$=lpad$(rtrm$(resp$(6)(1:8)),8) ! starting vendor number
00330   open #paymstr=1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&env$('cno')&",Shr",internal,input,keyed 
00340   open #trmstr=2: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&env$('cno')&",Shr",internal,input,keyed 
00350   if prtall=check_range then goto ASK_BANK_ETC else !:
          if prtall=print_all then goto ASK_FIRST_PAYEE
00360 ASK_VN: ! 
00370   fnTos(sn$="cllabel-2") !:
        respc=0 : mylen=20
00380   fnLbl(1,1,"Payee to Print:",mylen,right)
00390   fncombof("Payee",1,22,20,env$('Q')&"\CLmstr\Paymstr.h"&env$('cno'),1,8,9,20,env$('Q')&"\CLmstr\Payidx1.h"&env$('cno'),1,0, 'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"') !:
        resp$(respc+=1)=""
00400   fnCmdSet(3)
00410   fnAcs(sn$,0,mat resp$,ck) !:
        if ck=5 then goto END1 else !:
          vn$=lpad$(rtrm$(resp$(1)(1:8)),8)
00420   read #paymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey MSGBOX1
00430   goto L530
00440 ! ______________________________________________________________________
00450 ASK_FIRST_PAYEE: ! 
00460   if trim$(vn$)="" or trim$(vn$)="0" then goto READ_PAYMSTR
00470   vn$=lpad$(rtrm$(vn$),8) !:
        read #paymstr,using 'Form Pos 1,C 8,4*C 30',key>=vn$: vn$,nam$,ad1$,ad2$,csz$ eof END1
00480   goto L520
00490 ! ______________________________________________________________________
00500 READ_PAYMSTR: ! 
00510   read #paymstr,using 'Form Pos 1,C 8,4*C 30': vn$,nam$,ad1$,ad2$,csz$ eof END1
00520 L520: if prtall=check_range and curbal=0 then !:
          goto READ_PAYMSTR
00530 L530: gosub PRT
00540   if prtall=specific_payees then !:
          goto ASK_VN else goto READ_PAYMSTR
00550 ! ______________________________________________________________________
00560 END1: close #paymstr: 
00570   fnlabel(101,cap$,mat pt$,cp,nw)
00580 XIT: fnxit
00590 ! ______________________________________________________________________
00600 OPT2: ! 
00610   if wbc=0 then goto MSGBOX2 ! if failed to set bank account information
00620   tr4$=cnvrt$("N 2",wbc)&str$(1)&cnvrt$("N 8",c1) !:
        restore #trmstr,key>=tr4$: nokey EO_OPT2
00630 READ_TRMSTR: ! 
00640   read #trmstr,using 'Form POS 4,C 8,POS 28,C 8': ck$,vn$ eof EO_OPT2
00650   if ck$>lpad$(str$(c2),8) then goto EO_OPT2
00660   read #paymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey READ_TRMSTR
00670 PRT: ! 
00680   mat labeltext$=("")
00690   if printpayeenum$="True" then labeltext$(1)=vn$
00700   labeltext$(2)= nam$ !:
        labeltext$(3)=ad1$ !:
        labeltext$(4)=ad2$ !:
        labeltext$(5)=csz$
00710   if trim$(labeltext$(3))="" then labeltext$(3)=labeltext$(4) !:
          labeltext$(4)=""
00720   if trim$(labeltext$(4))="" then labeltext$(4)=labeltext$(5) !:
          labeltext$(5)=""
00730   fnaddlabel(mat labeltext$)
00740   if prtall=check_range then goto READ_TRMSTR
00750 EO_OPT2: return 
00760 ! ______________________________________________________________________
00770 ASK_BANK_ETC: ! 
00780   gosub OPT2
00790   goto END1
00800 ! ______________________________________________________________________
00810 ! <Updateable Region: ERTN>
00820 ERTN: fnerror(program$,err,line,act$,"xit")
00830   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00840   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00850   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00860 ERTN_EXEC_ACT: execute act$ : goto ERTN
00870 ! /region
00880 ! ______________________________________________________________________
00890 MSGBOX1: !  bad vendor #
00900   goto ASK_VN
00910 ! ______________________________________________________________________
00920 MSGBOX2: ! no check information entered
00930   goto MAIN
00940 ! ______________________________________________________________________
