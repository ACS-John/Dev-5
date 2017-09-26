00010 ! Replace S:\acsCL\TrToCPA
00020 ! Transfer to Accountant
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fndat,fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fnprg,fnstyp,fntos,fnlbl,fntxt,fnchk,fncmdset,fnacs,fnwait,fncomboa,fnmsgbox,fnchain
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,cnam$*40,de$*35,ta(2),td$*30,prd(23),srgln$(17)*12,sra(17)
00080   dim prgln$(15)*12,pra(15),r9$*104,cap$*128,glwk$*256,ml$(2)*70
00090   dim ty$(3)*20
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Transfer to Accountant")
00120   let cancel=99 : let right=1
00130   let fncno(cno,cnam$)
00140   let fndat(dat$,1)
00150   let ty$(1)=" Check Number:" !:
        let ty$(2)=" Deposit Number:" !:
        let ty$(3)=" Adjustment Number: "
00160 ! ______________________________________________________________________
00170 MAIN: ! 
00180   let fntos(sn$="Tr2cpa") !:
        let mylen=40 : let mypos=mylen+2 : let lc=0
00190   let fnlbl(lc+=1,1,"Starting Date:",mylen,right)
00200   let fntxt(lc,mypos,10,0,1,"1003",0,"Earliest transation date to be transferred") !:
        ! Let RESP$(1)=""
00210   let fnlbl(lc+=1,1,"Ending Date:",mylen,right)
00220   let fntxt(lc,mypos,10,0,1,"1003",0,"Last transation date to be transferred") !:
        ! Let RESP$(2)=""
00230   let lc+=1
00240   let fnchk(lc+=1,mypos,"Transfer previously posted transactions:",1) !:
        if resp$(3)="" then let resp$(3)="False"
00250   let lc+=1
00260   let fnlbl(lc+=1,1,"Destination Path:",mylen,right)
00270   let fntxt(lc,mypos,66) !:
        if resp$(4)="" then let resp$(4)="A:\"
00280   let fncmdset(2) !:
        let fnacs(sn$,0,mat resp$,ck)
00290   if ck=5 then goto XIT
00300   let d1=val(resp$(1)(5:6))*10000+val(resp$(1)(7:8))*100+val(resp$(1)(3:4)) ! beginning date !:
        let d2=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! ending date  ! convert dates back to mmddyy
00310   if resp$(3)(1:1)="T" then let pvt$="Y": else let pvt$="N" !:
          ! post previously entries
00320   let dv$=resp$(4)
00330   gosub GETPRC
00340 ! ______________________________________________________________________
00350   let fnwait
00360   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative  !:
        read #20,using 'Form POS 618,10*N 1': mat dedcode !:
        close #20: 
00370   let d2$=cnvrt$("PIC(######)",d2)
00380   open #20: "Name="&env$('Q')&"\GLmstr\glBucket.H"&str$(cno)&",Shr",internal,input,relative ioerr L400 !:
        read #20,using 'Form POS 1,N 1',rec=1: glb norec L390
00390 L390: close #20: 
00400 L400: if glb=2 then let glwk$=env$('Q')&"\GLmstr\GL"&d2$&".H"&str$(cno)
00410   if glb><2 then let glwk$=env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&str$(cno)
00420   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00430   open #tralloc=2: "Name="&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&str$(cno)&",Shr",internal,input,keyed 
00440   open #glwk101=3: "Name="&env$('Q')&"\CLmstr\GLWK101.H"&str$(cno)&",Size=0,RecL=104,Replace",internal,output 
00450   open #glwk201=4: "Name="&env$('Q')&"\CLmstr\GLWK201.H"&str$(cno)&",Size=0,RecL=110,Replace",internal,output 
00460   open #bankmstr=5: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00470   open #paymstr=6: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00480   let fnopenprn
00490   gosub HDR
00500 READ_TRMSTR: ! 
00510   read #trmstr,using 'Form POS 1,G 2,N 1,C 8,N 6,pd 10.2,C 8,C 35,N 1,X 6,N 1': bk$,cde,tr$,tr4,amt,ven$,de$,pcde,scd eof ENDALL
00520   if bk$><hbk$ then gosub CONTRA
00530   let hbk$=bk$
00540 ! IF SCD=2 THEN GOTO 'ASDF2'
00550   if amt=0 then goto READ_TRMSTR
00560   let tr$=lpad$(rtrm$(tr$),12)
00570   let pr1=0
00580   if scd=4 then let pr1=val(ven$) conv L590
00590 L590: if pvt$="Y" then goto L610
00600   if pcde=1 then goto READ_TRMSTR
00610 L610: if fndate_mmddyy_to_ccyymmdd(tr4)<fndate_mmddyy_to_ccyymmdd(d1) then goto READ_TRMSTR
00620   if fndate_mmddyy_to_ccyymmdd(tr4)>fndate_mmddyy_to_ccyymmdd(d2) then goto READ_TRMSTR
00630   gosub CKALLOC
00640   if tal=amt then goto L660
00650   print #255: "Bank Code: "&bk$&ty$(cde)&ltrm$(tr$)&" Total: "&ltrm$(cnvrt$("N 10.2",amt))&" Allocations: "&ltrm$(cnvrt$("N 10.2",tal))&" Entry Skipped"
00660 L660: let amt=typ=0
00670   read #paymstr,using 'Form POS 134,N 2',key=ven$: typ nokey L680
00680 L680: if typ=0 then let ven$=""
00690   let bgl$=""
00700   read #bankmstr,using 'Form POS 33,C 12',key=bk$: bgl$ nokey L710
00710 L710: let key$=bk$&str$(cde)&tr$(5:12) !:
        restore #tralloc,key=key$: nokey EO_TRALLOC
00720 READ_TRALLOC: ! 
00730   read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,tr5,td$,ivd eof EO_TRALLOC !:
        if key$<>newkey$ then goto EO_TRALLOC
00740   let td$=de$(1:30) ! SEND NAME TO GL INSTEAD OF DESCRIPTION
00750   if tr5=0 then goto READ_TRALLOC
00760   let tr6=cde
00770   if tr6>1 then let tr5=-tr5
00780   if prc$="Y" and scd=4 then goto COMBINEPR
00790 WRITE_GLWK101: ! 
00800   write #glwk101,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gl$,tr4,tr5,tr6,0,tr$,td$,ven$,"","","",bgl$
00810 AFTER_WRITE_GLWK101: ! 
00820   let amt+=tr5
00830   if tr6=3 then !:
          write #glwk101,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$,tr4,-tr5,tr6,0,tr$,td$,ven$,"","","",bgl$
00840   if pr1=0 then goto L920
00850   if ivd=1 then let prd(4)+=tr5
00860   if ivd>1 and ivd<5 then let prd(ivd+3)=-tr5
00870   if ivd=15 then let prd(8)=-tr5
00880   if ivd>4 and ivd<15 and dedcode(ivd-4)=2 then let tr5=-tr5
00890   if ivd>4 and ivd<15 then let prd(ivd+4)=-tr5
00900   if ivd=16 then let prd(19)=-tr5
00910   if fp(ivd*.01)=.19 then let prd(20)=int(ivd*.01)
00920 L920: goto READ_TRALLOC
00930 ! ______________________________________________________________________
00940 EO_TRALLOC: ! 
00950   rewrite #trmstr,using 'Form POS 71,N 1': 1
00960   if pr1=0 then goto L1010
00970   let prd(1)=pr1 : let prd(2)=tr4
00980   let prd(3)=val(tr$) conv L990
00990 L990: let prd(22)=amt
01000   write #glwk201,using 'Form POS 1,N 4,2*PD 4,19*PD 5.2,PD 3': mat prd
01010 L1010: mat prd=(0)
01020   if cde=2 then let p1=68 else let p1=56
01030   if cde=3 and amt>0 then let t1+=amt: let t2=t2-amt: goto L1080
01040   if cde=3 and amt<0 then let t2+=amt: let t1=t1-amt: goto L1110
01050   if cde=2 then !:
          let c2+=amt: let t2+=amt else let c1+=amt: let t1+=amt
01060   print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(tr$),tr4,de$,amt pageoflow NEWPGE
01070   goto THERE
01080 L1080: print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(tr$),tr4,de$,amt pageoflow NEWPGE: let p1=68
01090   print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(tr$),tr4,de$,-amt pageoflow NEWPGE
01100   goto THERE
01110 L1110: print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(tr$),tr4,de$,-amt pageoflow NEWPGE: let p1=68
01120   print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(tr$),tr4,de$,amt pageoflow NEWPGE
01130 THERE: ! 
01140   goto READ_TRMSTR
01150 ! ______________________________________________________________________
01160 NEWPGE: ! 
01170   print #255: newpage
01180   gosub HDR
01190   continue 
01200 ! ______________________________________________________________________
01210 HDR: ! 
01220   print #255,using 'Form POS 1,C 8,CC 76': date$,cnam$
01230   print #255,using 'Form POS 1,C 8,POS 31,C 40': time$,"Transferred to General Ledger"
01240   print #255,using 'Form POS 1,C 4,N 4,CC 76': "Page",pg+=1,dat$ !:
        print #255: ""
01250   print #255: "Ref-Numb    Date    Payee/Description                      Checks     Deposits"
01260   print #255: "________  ________  ___________________________________  __________  __________"
01270   return 
01280 ! ______________________________________________________________________
01290 ENDALL: ! 
01300   if pri1=1 then gosub PRWRITE
01310   gosub CONTRA
01320   print #255: tab(56);"  ____________  ____________"
01330   print #255,using 'Form POS 56,2*N 14.2': t1,t2
01340   let fncloseprn
01350   close #1: 
01360   close #tralloc: 
01370   close #glwk101: 
01380   let lr4=lrec(glwk201)
01390   close #glwk201: 
01400   close #bankmstr: 
01410   if trim$(dv$)="" then goto TRY_TO_SEND_TO_GL
01420   execute "Copy "&env$('Q')&"\CLmstr\GLWk101.h"&str$(cno)&" "&dv$&" -n" ioerr MSGBOX1
01430   execute "Copy "&env$('Q')&"\CLmstr\GLWk201.h"&str$(cno)&" "&dv$&" -n"
01440   execute "Copy "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&" "&dv$&" -n"
01450   execute "Copy "&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&" "&dv$&" -n"
01460 XIT: let fnxit
01470 ! ______________________________________________________________________
01480 TRY_TO_SEND_TO_GL: ! 
01490   if glb=2 then goto BUCKET
01500   execute "Copy "&env$('Q')&"\CLmstr\GLWK101.H"&str$(cno)&' '&env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".H"&str$(cno)&" -n"
01510   execute "Copy "&env$('Q')&"\CLmstr\GLWK201.H"&str$(cno)&' '&env$('Q')&"\GLmstr\GLWK2"&wsid$&".H"&str$(cno)&" -n"
01520   if lr4=0 then goto L1550
01530   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L1550
01540   let fnprg("S:\acsGL\PRMerge",2) !:
        let fnstyp(99)
01550 L1550: let fnchain("S:\acsGL\acglMrge")
01560 ! ______________________________________________________________________
01570 CONTRA: let t9$="999999999999"
01580   if trim$(dv$)><"" then goto L1670
01590   if c1=0 then goto L1630
01600   let td$="CHECKS   "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&" - "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)
01610   write #glwk101,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$,d2,-c1,1,0,t9$,td$,"","","","",bgl$
01620   let p1=68: let t2=t2-c1: !:
        print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(t9$)(1:10),d2,td$(1:35),-c1 pageoflow NEWPGE
01630 L1630: if c2=0 then goto L1670
01640   let td$="DEPOSITS "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&" - "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)
01650   write #glwk101,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$,d2,-c2,2,0,t9$,td$,"","","","",bgl$
01660   let p1=56: let t1=t1-c2: !:
        print #255,using 'Form POS 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,POS P1,N 13.2': ltrm$(t9$)(1:10),d2,td$(1:35),-c2 pageoflow NEWPGE
01670 L1670: let c1=c2=0
01680   return 
01690 ! ______________________________________________________________________
01700 GETPRC: ! 
01710   let pri1=0
01720   open #7: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input ioerr L1830 !:
        read #7,using 'Form POS 437,15*C 12': mat prgln$ !:
        close #7: 
01730   let pri1=1
01740   print newpage
01750   close #103: ioerr L1760
01760 L1760: open #103: "SROW=9,SCOL=09,EROW=14,ECOL=70,BORDER=SR,Caption=<"&cap$,display,outin 
01770   mat ml$(4) : mat ml$=("")
01780   let ml$(1)="This program can combine all like General Ledger Numbers for"
01790   let ml$(2)="Payroll Withholding Accounts as they are transferred to GL."
01800   let ml$(4)="Do you wish to combine these accounts?"
01810   let fnmsgbox(mat ml$,resp$,cap$,3)
01820   if resp$='Cancel' then goto XIT else let prc$=resp$(1:1)
01830 L1830: return 
01840 ! ______________________________________________________________________
01850 COMBINEPR: ! 
01860   for j=1 to 15
01870     if prgln$(j)=gl$ then goto L1900
01880   next j
01890   goto WRITE_GLWK101
01900 L1900: let pra(j)=pra(j)+tr5
01910   goto AFTER_WRITE_GLWK101
01920 ! ______________________________________________________________________
01930 PRWRITE: ! 
01940   let td$="PR-WH: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&" - "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)
01950   for j=1 to 15
01960     if pra(j)=0 then goto L1980
01970     write #glwk101,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': prgln$(j),d2,pra(j),1,0,str$(d2),td$,"","","","",bgl$
01980 L1980: next j
01990   return 
02000 ! ______________________________________________________________________
02010 CKALLOC: ! 
02020   let tal=0
02030   let key$=bk$&str$(cde)&tr$(5:12) !:
        restore #tralloc,key=key$: nokey EO_CKALLOC
02040 CKALLOC_READ_TRALLOC: ! 
02050   read #tralloc,using 'Form Pos 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,tr5,td$,ivd eof EO_CKALLOC
02060   if key$<>newkey$ then !:
          goto EO_CKALLOC else !:
          let tal+=tr5 !:
          goto CKALLOC_READ_TRALLOC
02070 EO_CKALLOC: ! 
02080   return 
02090 ! ______________________________________________________________________
02100 BUCKET: ! MOVE TO GLBUCKET
02110   open #glwk101=3: "Name="&env$('Q')&"\CLmstr\GLWK101.H"&str$(cno),internal,input 
02120   open #9: "Name="&glwk$&",RecL=104,USE",internal,output 
02130 L2130: read #glwk101,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gl$, tr4,tr5,tr6,tr7, tr$,td$,ven$,j$,j$,j$,bgl$ eof L2160
02140   write #9,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gl$,tr4,tr5,tr6,tr7,tr$,td$,ven$,j$,j$,j$,bgl$
02150   goto L2130
02160 L2160: open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&str$(cno)&",Shr",internal,outin,keyed ioerr XIT
02170   let fnchain("S:\acsGL\PRMerge")
02180 ! ______________________________________________________________________
02190 ! <Updateable Region: ERTN>
02200 ERTN: let fnerror(program$,err,line,act$,"xit")
02210   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02230   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02240 ERTN_EXEC_ACT: execute act$ : goto ERTN
02250 ! /region
02260 ! ______________________________________________________________________
02270 MSGBOX1: ! 
02280   mat ml$(2) !:
        let ml$(1)="Make sure the diskette is properly inserted " !:
        let ml$(2)="and the proper device has been selected." !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MAIN
02290 ! ______________________________________________________________________
