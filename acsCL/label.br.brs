00010 ! Replace S:\acsCL\Label
00020 ! pr labels for payees
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnaddlabel,fnlabel,fntos,fnlbl,fncomboa,fnchk,fncmdset,fnacs,fntxt,fncombof
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,cap$*128,io2$(3),wrd2$(3)
00080   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,prtbegin$*5
00090   dim labeltext$(5)*120,pt$(5),message$*40,item1$(3),resp$(10)*30
00100 ! ______________________________________________________________________
00110   let fncno(cno,cnam$)
00120   let fntop(program$,cap$="Payee Labels")
00130   cancel=99 : let right=1 : let limit_to_list=1 : let on=1 !:
        let off=0 : let left=0 : center=2
00140 ! ______________________________________________________________________
00150 MAIN: ! 
00160   let fntos(sn$="cllabel-1")
00170   let respc=0 : let mylen=25 : let mypos=mylen+2
00180   let fnlbl(1,1,"Print Labels For:",mylen,right)
00190   let fi$="cllabels" !:
        let item1$(print_all=1)="[All]" !:
        let item1$(check_range=2)="Range of Checks" !:
        let item1$(specific_payees=3)="Specific payees" !:
        let fncomboa(fi$,1,mypos,mat item1$,"The labels can be printed in Customer Number order,Customer Name order, or in Bar Code sequence") !:
        let resp$(respc+=1)=item1$(1)
00200   let fnchk(2,mypos+2,'Print Payee Number on Label',right) !:
        let resp$(respc+=1)='False'
00210   let fnlbl(4,1,"Bank:",mylen,right)
00220   let fncombof('Bank',4,mypos,33,env$('Q')&"\CLmstr\BankMstr.h"&str$(cno),1,2,3,30,env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno),limit_to_list) !:
        let resp$(respc+=1)=""
00230   let fnlbl(5,1,"Starting Check Number:",25,1)
00240   let fncombof('Check',5,mypos,33,env$('Q')&"\CLmstr\TrMstr.h"&str$(cno),4,8,36,35) !:
        let resp$(respc+=1)=""
00250   let fnlbl(6,1,"Ending Check Number:",25,1)
00260   let fncombof('Check',6,mypos,33,env$('Q')&"\CLmstr\TrMstr.h"&str$(cno),4,8,36,35) !:
        let resp$(respc+=1)=""
00270   let fnlbl(8,1,"Starting Payee Number:",25,1)
00280 ! Let FNTXT(8,27,8,0,1,"",0,'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"')! !:
        ! Let RESP$(RESPC+=1)=""
00290   let fncombof("Payee",8,27,20,env$('Q')&"\CLmstr\Paymstr.h"&str$(cno),1,8,9,20,env$('Q')&"\CLmstr\Payidx1.h"&str$(cno),1,0, "Select starting payee record for printing") !:
        let resp$(respc+=1)=""
00300   let fncmdset(2)
00310   let fnacs(sn$,0,mat resp$,ck) !:
        if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then let prtall=print_all else !:
            if resp$(1)=item1$(2) then let prtall=check_range else !:
              if resp$(1)=item1$(3) then let prtall=specific_payees
00320   let printpayeenum$=resp$(2) !:
        let wbc=val(resp$(3)(1:2)) ! working bank code !:
        c1=val(resp$(4)(1:8)) ! starting check number !:
        c2=val(resp$(5)(1:8)) ! ending check number !:
        let vn$=lpad$(rtrm$(resp$(6)(1:8)),8) ! starting vendor number
00330   open #paymstr=1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00340   open #trmstr=2: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00350   if prtall=check_range then goto ASK_BANK_ETC else !:
          if prtall=print_all then goto ASK_FIRST_PAYEE
00360 ASK_VN: ! 
00370   let fntos(sn$="cllabel-2") !:
        let respc=0 : let mylen=20
00380   let fnlbl(1,1,"Payee to Print:",mylen,right)
00390   let fncombof("Payee",1,22,20,env$('Q')&"\CLmstr\Paymstr.h"&str$(cno),1,8,9,20,env$('Q')&"\CLmstr\Payidx1.h"&str$(cno),1,0, 'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"') !:
        let resp$(respc+=1)=""
00400   let fncmdset(3)
00410   let fnacs(sn$,0,mat resp$,ck) !:
        if ck=5 then goto END1 else !:
          let vn$=lpad$(rtrm$(resp$(1)(1:8)),8)
00420   read #paymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey MSGBOX1
00430   goto L530
00440 ! ______________________________________________________________________
00450 ASK_FIRST_PAYEE: ! 
00460   if trim$(vn$)="" or trim$(vn$)="0" then goto READ_PAYMSTR
00470   let vn$=lpad$(rtrm$(vn$),8) !:
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
00570   let fnlabel(101,cap$,mat pt$,cp,nw)
00580 XIT: let fnxit
00590 ! ______________________________________________________________________
00600 OPT2: ! 
00610   if wbc=0 then goto MSGBOX2 ! if failed to set bank account information
00620   let tr4$=cnvrt$("N 2",wbc)&str$(1)&cnvrt$("N 8",c1) !:
        restore #trmstr,key>=tr4$: nokey EO_OPT2
00630 READ_TRMSTR: ! 
00640   read #trmstr,using 'Form POS 4,C 8,POS 28,C 8': ck$,vn$ eof EO_OPT2
00650   if ck$>lpad$(str$(c2),8) then goto EO_OPT2
00660   read #paymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey READ_TRMSTR
00670 PRT: ! 
00680   mat labeltext$=("")
00690   if printpayeenum$="True" then let labeltext$(1)=vn$
00700   let labeltext$(2)= nam$ !:
        let labeltext$(3)=ad1$ !:
        let labeltext$(4)=ad2$ !:
        let labeltext$(5)=csz$
00710   if trim$(labeltext$(3))="" then let labeltext$(3)=labeltext$(4) !:
          let labeltext$(4)=""
00720   if trim$(labeltext$(4))="" then let labeltext$(4)=labeltext$(5) !:
          let labeltext$(5)=""
00730   let fnaddlabel(mat labeltext$)
00740   if prtall=check_range then goto READ_TRMSTR
00750 EO_OPT2: return 
00760 ! ______________________________________________________________________
00770 ASK_BANK_ETC: ! 
00780   gosub OPT2
00790   goto END1
00800 ! ______________________________________________________________________
00810 ! <Updateable Region: ERTN>
00820 ERTN: let fnerror(program$,err,line,act$,"xit")
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
