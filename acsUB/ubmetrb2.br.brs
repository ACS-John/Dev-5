00010 ! Replace S:\acsUB\ubMetRB2
00020 ! pr Complete Route Sheets
00030 ! ______________________________________________________________________
00040   library "S:\Core\Library": fnacs,fnlbl,fnwait,fncmbrt2,fntos,fnopenprn,fncloseprn,fnerror,fncno,fnxit,fndat,fncno,fncmdset,fntop,fncomboa,fnpause,fnfra,fnopt,fnchk,fncmbact,fncmdkey,fnchain,fnpa_finis,fnpa_newpage,fnpa_open
00050   on error goto ERTN
00065 ! if env$('client')="Eldorado" then let fnchain('S:\acsUB\ubmetrb2_eldorado')
00070 ! ______________________________________________________________________
00080   dim z$*10,e$(4)*30,x$*10,f$(1)*12,f$(3)*12,cnam$*40,cap$*128,dat$*20
00090   dim z2$*10,e2$(4)*30,f12$*12,f32$(12),f22$(12),a(7),a2(7)
00100   dim snm$(10)*20,a(7),option$(5),extra(13),service$(3)*26,ms$(13)*3
00110   dim txt$*50,resp$(9)*50
00120 ! ______________________________________________________________________
00130   let fncno(cno,cnam$) !:
        ! 
00140   let fntop("S:\acsUB\ubMetrB2",cap$="Route Book Pages")
00150   let fndat(dat$,1)
00160   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed 
00170   open #11: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00180 ! ______________________________________________________________________
00190 ! this section+the comboA on the first screen is just what you need !:
        ! for a fnCurrently availableServiceTypeComboBox
00200   let ms$(1)="DEC"
00210   let ms$(2)="NOV"
00220   let ms$(3)="OCT"
00230   let ms$(4)="SEP"
00240   let ms$(5)="AUG"
00250   let ms$(6)="JUL"
00260   let ms$(7)="JUN"
00270   let ms$(8)="MAY"
00280   let ms$(9)="APR"
00290   let ms$(10)="MAR"
00300   let ms$(11)="FEB"
00310   let ms$(12)="JAN"
00320   let ms$(13)="DEC"
00330   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using "Form POS 1,10*C 20,10*C 2",rec=1: mat snm$,mat srv$ !:
        close #20: 
00340   let x=0
00350   for j=1 to 4
00360     if j=1 and trim$(snm$(j))="Water" then let option$(x+=1)=srv$(j) !:
            let service=service+1
00370     if j=3 and trim$(snm$(j))="Electric" then let option$(x+=1)=srv$(j) !:
            let service=service+1
00380     if j=3 and trim$(snm$(j))="Lawn Meter" then let option$(x+=1)=srv$(j) !:
            let service=service+1
00390     if j=4 and trim$(snm$(j))="Gas" then let option$(x+=1)=srv$(j) !:
            let service=service+1
00400   next j
00410   let option$(x+=1)=" "
00420   mat option$(x)
00430   mat service$(service)
00440   let service =udim(service$)
00450 ! ______________________________________________________________________
00460 MENU1: ! 
00470   let fntos(sn$="ubMetrRt") !:
        let mylen=35 : let mypos=mylen+3 : let respc=lc=0
00480   let fnlbl(lc+=1,1,"Route Number:",mylen,1)
00490   let fncmbrt2(lc,mypos) !:
        let resp$(1)="1"
00500   let fnlbl(lc+=1,1,"Service Type - 1st Service:",mylen,1)
00510   let fncomboa("ubrate3",lc,mypos,mat option$) !:
        let resp$(2)=option$(1)
00520   let fnlbl(lc+=1,1,"Service Type - 2nd Service:",mylen,1)
00530   let fncomboa("ubrate3",lc,mypos,mat option$) !:
        let resp$(3)=option$(1)
00540   let fnfra(5,1,4,45,"Single Wide or Double Wide","Allow one or two customers per page.",0)
00550   let fnopt(1,2,"Singe Wide",0,1) !:
        let resp$(4)="True"
00560   let fnopt(2,2,"Double Wide - Different customers",0,1) !:
        let resp$(5)="False"
00570   let fnopt(3,2,"Double Wide - Different services",0,1) !:
        let resp$(6)="False"
00580   if env$('client')="Franklinton" or env$('client')="Divernon" then let resp$(4)="False": let resp$(6)="True"
00590   let fnchk(11,28,"Select Accounts to Print:",1) !:
        let resp$(7)="False"
00600   let fnfra(13,1,2,45,"Option for printing","The system can pr the actual form or just fill in the blanks on a pre-printed form.",0)
00610   let fnopt(1,2,"Print complete form",0,2) !:
        let resp$(8)="True"
00620   let fnopt(2,2,"Fill in the blanks",0,2)
00625   if env$('client')="Carrizo" then let resp$(9)="True" else let resp$(9)="False"
00630   let fncmdset(3) !:
        let fnacs(sn$,0,mat resp$,ck)
00640   if ck=5 then goto XIT
00650   if uprc$(resp$(1))=uprc$("[All]") then let route=0 else !:
          let route=val(resp$(1))
00660   let svt$=resp$(2)
00670   let svt2$=resp$(3)
00680   if resp$(4)="True" then let width=1
00690   if resp$(5)="True" then let width=2
00700   if resp$(6)="True" then let width=3
00710   if resp$(7)="True" then let selectone=1 else let selectone=0
00720   if resp$(8)="True" then let formoption=1 ! complete form
00730   if resp$(9)="True" then let formoption=2 ! fill in blanks
00740   let service=1 ! only printing one service if not                                                answered as all
00750   for j=1 to 10
00760     if svt$=trim$(srv$(j)) then let service$=snm$(j)(1:10)
00770     if svt2$=trim$(srv$(j)) then let service2$=snm$(j)(1:10)
00780   next j
00790 ! ______________________________________________________________________
00800   on fkey 5 goto DONE
00810   if formoption=2 then let fnpa_open : goto L830
00820   let fnopenprn
00830 L830: if selectone=1 then goto SELECTONE
00840 LOOP_TOP: ! 
00850   let x=0
00860 ! print
00870 L870: read #1,using L1060: z$,mat e$,f1$,d1,d3,f3$,c4,f2$,d5,d7,d9,d11,book1,mat a,d13,extra16,b2,b5 eof DONE
00880   let z2$=f12$="" : let d12=d32=d52=d72=d92=d112=c42=0 : mat e2$=("")
00890   if route>0 and book1<>route then goto L870
00900   if width=1 or width=3 then goto L920
00910   read #1,using L1060: z2$,mat e2$,f12$,d12,d32,f32$,c42,f22$,d52,d72,d92,d112,book2,mat a2 eof L1060
00920 L920: if svt$=srv$(1) and width=1 then let previous=d1: columnonename$=srv$(1): let firstmeter$=f1$: let secondmeter$="": let firstcode$=str$(a(1)): let secondcode$="" ! water only service
00930   if svt$=srv$(1) and width=2 then let previous=d1: let previous2=d12: let firstmeter$=f1$: let secondmeter$=f12$: let firstcode$=str$(a(1)): let secondcode$=str$(a2(1)) ! two different water customers
00940   if svt$=srv$(1) and svt2$=srv$(3) and width=3 then let previous=d1: let previous2=d5 : let firstmeter$=f1$: let secondmeter$=f2$: let firstcode$=str$(a(1)): let secondcode$=str$(a(3)) ! water and electric
00950   if svt$=srv$(1) and svt2$=srv$(4) and width=3 then let previous=d1: let previous2=d9 : let firstmeter$=f1$: let secondmeter$=f3$: let firstcode$=str$(a(1)): let secondcode$=str$(a(4)) ! water and gas
00960   if svt$=srv$(3) and width=1 then let previous=d5: let firstmeter$=f2$: let secondmeter$="": let firstcode$=str$(a(3)): let secondcode$="" ! electric only
00970   if svt$=srv$(3) and width=2 then let previous=d5: let previous2=d52: let firstmeter$=f2$: let secondmeter$=f22$: let firstcode$=str$(a(3)): let secondcode$=str$(a2(3)) ! electric or lawn meter
00980   if svt$=srv$(3) and svt2$=srv$(1) and width=3 then let previous=d5: let previous2=d1: let firstmeter$=f2$: let secondmeter$=f1$: let firstcode$=str$(a(3)): let secondcode$=str$(a(1)) ! electric or lawn meter  and water
00990   if svt$=srv$(3) and svt2$=srv$(4) and width=3 then let previous=d5: let previous2=d9: let firstmeter$=f2$: let secondmeter$=f3$: let firstcode$=str$(a(3)): let secondcode$=str$(a(4)) ! electric or lawn meter  and gas
01000   if svt$=srv$(4) and width=1 then let previous=d9: let firstmeter$=f3$: let secondmeter$="": let firstcode$=str$(a(4)): let secondcode$="" ! gas only
01010   if svt$=srv$(4) and width=2 then let previous=d9: let previous2=d92: let firstmeter$=f3$: let secondmeter$=f32$: let firstcode$=str$(a(4)): let secondcode$=str$(a2(4)) ! gas
01020   if svt$=srv$(4) and svt2$=srv$(1) and width=3 then let previous=d9: let previous2=d1: let firstmeter$=f3$: let secondmeter$=f1$: let firstcode$=str$(a(4)): let secondcode$=str$(a(1)) ! gas and water
01030   if svt$=srv$(4) and svt2$=srv$(3) and width=3 then let previous=d9: let previous2=d5: let firstmeter$=f3$: let secondmeter$=f2$: let firstcode$=str$(a(4)): let secondcode$=str$(a(3)) ! gas and electric
01040   if route=0 or route=book2 then goto L1060
01050   let z2$=f12$="" : let d12=d32=d52=d72=d92=d112=c42=0 : mat e2$=("")
01060 L1060: form pos 1,c 10,4*c 30,c 12,pos 217,pd 5,pos 227,pd 5,pos 373,c 12,pos 213,pd 4,pos 361,c 12,pos 237,pd 5,pos 247,pd 5,pos 257,pd 5,pos 267,pd 5,pos 1741,n 2,pos 143,7*pd 2,pos 277,pd 5,pos 1818,n 3,pos 161,pd 4.2,pos 173,pd 4.2
01070   if formoption=2 then gosub BLANKS : goto L1700
01080   if width=2 or width=3 then for j=1 to 2: pr #255,using L1320: "|": next j
01090   if width=1 then for j=1 to 2: pr #255,using L1320: "|": next j
01100   if width=1 then pr #255,using L1160: cnam$(1:37),"|"
01110   if width=2 or width=3 then pr #255,using L1160: cnam$(1:37),"|",cnam$(1:37)
01120   if width=1 then pr #255,using L1160: service$
01130   if width=2 then pr #255,using L1160: service$,"|",service$
01140   if width=3 then pr #255,using L1160: service$,"|",service2$
01150 ! 
01160 L1160: form pos 2,cc 37,pos 45,c 1,x 1,cc 37,skip 1
01170   if width=2 or width=3 then for j=1 to 2: pr #255,using L1320: "|": next j
01180   if width=1 then for j=1 to 2: pr #255,using L1320: "|": next j
01190   form pos 14,c 12,pos 45,c 1,pos 54,c 12,skip 1
01200   if width=1 then pr #255,using L1290: e$(1),"|"
01210   if width=2 then pr #255,using L1290: e$(1),"|",e2$(1)
01220   if width=3 then pr #255,using L1290: e$(1),"|"
01230   if width=1 then pr #255,using L1290: "Meter #: "&firstmeter$,"|"
01240   if width=2 then pr #255,using L1290: "Meter #: "&firstmeter$,"|","Meter #: "&firstmeter$
01250   if width=3 then pr #255,using L1290: "Meter #: "&firstmeter$,"|","Meter #: "&secondmeter$
01260   if width=1 then pr #255,using L1290: "Rate Code: "&firstcode$,"|"
01270   if width=2 then pr #255,using L1290: "Rate Code: "&firstcode$,"|","Rate Code: "&firstcode$
01280   if width=3 then pr #255,using L1290: "Rate Code: "&firstcode$,"|","Rate Code: "&secondcode$
01290 L1290: form pos 10,c 30,pos 45,c 1,pos 50,c 30,skip 1
01300   if width=2 or width=3 then for j=1 to 5: pr #255,using L1320: "|": next j
01310   if width=1 then for j=1 to 5: pr #255,using L1320: "|": next j
01320 L1320: form pos 45,c 1,skip 1
01330   if width=2 or width=3 then pr #255,using L1340: year$,"|",year$ else pr #255,using L1340: year$,"|"
01340 L1340: form pos 2,c 4,pos 45,c 1,x 1,c 4,skip 1
01350 L1350: form pos 6,c 80
01360 ! 
01370   if width=1 then pr #255,using L1350: "=======================================|"
01380   if width=2 or width=3 then pr #255,using L1350: "=======================================|======================================="
01390   if width=1 then pr #255,using L1350: "Dat|  READINGS  |CONSUMPTION|  REMARKS |"
01400   if width=2 or width =3 then pr #255,using L1350: "Dat|  READINGS  |CONSUMPTION|  REMARKS |Dat|  READINGS  |CONSUMPTION|  REMARKS "
01410   if width=1 then pr #255,using L1350: "___|____________|___________|__________|"
01420   if width=2 or width=3 then pr #255,using L1350: "___|____________|___________|__________|___|____________|___________|__________"
01430   for j=1 to 12
01440     if width=2 or width=3 then pr #255,using L1350: ms$(j)&"|            |           |          |"&ms$(j)&"|            |           |          "
01450     if width=1 then pr #255,using L1350: ms$(j)&"|            |           |          |"
01460     if width=2 or width=3 then pr #255,using L1350: "___|____________|___________|__________|___|____________|___________|__________"
01470     if width=1 then pr #255,using L1350: "___|____________|___________|__________|"
01480   next j
01490   if width=1 then pr #255,using L1510: ms$(13)&"|",previous,"| PREVIOUS  |","|"
01500   if width=2 or width=3 then pr #255,using L1510: ms$(13)&"|",previous,"| PREVIOUS  |","|"&ms$(13)&"|",previous2,"| PREVIOUS  |"
01510 L1510: form pos 6,c 4,pic(zzzzzzzzzzzz),pos 22,c 14,pos 45,c 5,pic(zzzzzzzzzzzz),c 14,skip 1
01520   if width=1 then pr #255,using L1350: "   |            | READING   |          |   | "
01530   if width=2 or width=3 then pr #255,using L1350: "   |            | READING   |          |   |            | READING   |          "
01540   if fbc><0 then let e$(2)="Disconnect"
01550   if width=1 then pr #255,using L1350: "=======================================|"
01560   if width=2 or width=3 then pr #255,using L1350: "=======================================|======================================="
01570   if width=1 then pr #255,using L1660: "|","                "," ACCOUNT  ","|"
01580   if width=2 then pr #255,using L1660: "|","                "," ACCOUNT  ","|","                "," ACCOUNT  ","|"
01590   if width=3 then pr #255,using L1660: "|","                "," ACCOUNT  ","|"
01600   if width=1 or width=3 then pr #255,using L1670: e$(2)(1:25),z$,"|"
01610   if width=2 then pr #255,using L1670: e$(2)(1:25),z$,"|",e2$(2)(1:25),z2$
01620   if width=1 or width=3 then pr #255,using L1670: e$(3)(1:25),"","|"
01630   if width=2 then pr #255,using L1670: e$(3)(1:25),"","|",e2$(3)(1:25),""
01640   if width=1 or width=3 then pr #255,using L1670: e$(4)(1:25),"","|"
01650   if width=2 then pr #255,using L1670: e$(4)(1:25),"","|",e2$(4)(1:25),""
01660 L1660: form pos 45,c 1,skip 1,pos 6,c 25,c 10,pos 45,c 1,pos 47,c 25,c 10,skip 1,pos 45,c 1,skip 1
01670 L1670: form pos 6,c 25,c 10,pos 45,c 1,pos 47,c 25,c 10,skip 1
01680   for j=1 to 3: pr #255,using L1320: "|": next j
01690   pr #255: newpage
01700 L1700: if selectone=1 then goto SELECTONE
01710   goto LOOP_TOP
01720 ! ______________________________________________________________________
01730 SELECTONE: ! 
01740   let sn$ = "Selectone" !:
        let fntos(sn$)
01750   let txt$="Account:" !:
        let fnlbl(1,1,txt$,16,1)
01760 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040
01770   if trim$(z$)<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          let fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          let fnlbl(3,1,txt$,44,1)
01780   let fncmbact(1,18) ! !:
        let resp$(1)=a$
01790   let fncmdkey("&Next",1,1,0,"Accept this record for printing") !:
        let fncmdkey("&Complete",5,0,1,"Print all selected records")
01800   let fnacs(sn$,0,mat resp$,ck)
01810   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto DONE
01820   if ck=5 then goto DONE
01830 ! 
01840   read #11,using L1060,key=a$: z$,mat e$,f1$,d1,d3,f3$,c4,f2$,d5,d7,d9,d11,book1,mat a,d13,extra16,b2,b5 nokey SELECTONE
01850   form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
01860   goto L920
01870 ! ______________________________________________________________________
01880 ! ______________________________________________________________________
01890 DONE: ! 
01900   close #1: ioerr L1910
01905   if formoption=2 then gosub RELEASE_PRINT: goto XIT
01910 L1910: let fncloseprn
01920 XIT: ! 
01930   let fnxit
01940 ! ______________________________________________________________________
01950 ! <Updateable Region: ERTN>
01960 ERTN: let fnerror(program$,err,line,act$,"xit")
01970   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02000 ERTN_EXEC_ACT: execute act$ : goto ERTN
02010 ! /region
02020 ! ______________________________________________________________________
02120 RELEASE_PRINT: ! 
02130   close #1: ioerr ignore
02140   close #3: ioerr ignore
02160   let fnpa_finis
02180   goto XIT
02190 IGNORE: continue 
02200 BLANKS: ! fill in blanks using prace
02210   pr #20: 'Call Print.MyFontSize(10)'
02230   column1=30 !:
        column2=90 !:
        column3=150
02240   let lyne=137
02250   let txt$=f1$ !:
        let lyne+=8.5 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02260   let txt$=f3$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
02270   let txt$=cnvrt$("pic(ZZZZZZ)",d13) !:
        let lyne+=8.5 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+11)&','&str$(lyne)&')'
02280   let txt$=cnvrt$("pic(zzz)",extra16) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
02290   let txt$=cnvrt$("pic(zzzzzzZZZZZZZZZ.zz)",b2) !:
        let lyne+=8.5 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02300   let txt$=cnvrt$("pic(zZZZZZZZZZ.zz)",b5) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
02305   pr #20: 'Call Print.MyFontSize(14)'
02310   let txt$=e$(2) !:
        let lyne+=52 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
02320   let txt$=e$(3) !:
        let lyne+=20 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
02330   let txt$=e$(4) !:
        let lyne+=5 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
02335   let txt$=e$(1) !:
        let lyne+=14 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+8)&','&str$(lyne)&')'
02340   let txt$=z$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-16)&','&str$(lyne-36)&')'
02342   pr #20: 'Call Print.MyFontSize(10)'
02350   let fnpa_newpage
02360   return 
