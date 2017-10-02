00010 ! Replace S:\acsPR\newjcRptS1
00020 ! Create Job Cost Report Program
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fncno,fnerror,fntop, fnxit, fnrx
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rn$*2,rt$*51,ch$(2)*132,psc(100),cap$*128,message$*40
00080   dim f$(20)*50,pp(20),ppr(20),dp(20),fc(20),tcj(20),tcs(20),ty$(24)*8
00090   dim a$(20)*32,a(20),underlin$*30
00100   dim ln$*255,pf$*255,af$*255,gpf$*255,gaf$*255,jpf$*255,jaf$*255,upf$*255
00110   dim uaf$*255
00120 ! ______________________________________________________________________
00121   let fntop("S:\acsPR\newjcRptS1",cap$="User Designed Reports (1)")
00130   let fncno(cno)
00150 ! ______________________________________________________________________
00160   data "JN"
00170   data "n$"
00180   data "A$(1)"
00190   data "a$(2)"
00200   data "a$(3)"
00210   data "X6"
00220   data "X7"
00230   data "X8"
00240   data "X9"
00250   data "cn"
00260   data "k$"
00270   data "X12"
00280   data "X13"
00290   data "X14"
00300   data "X15"
00310   data "X16"
00320   data "X17"
00330   data "X18"
00340   data "X19"
00350   data "X20"
00360   data "X21"
00370   data "X22"
00380   data "X23"
00390   data "X24"
00400   read mat ty$
00410 ! ___________________________
00420   let rn=fnrx
00430   let underlin$="______________________________"
00440   cap$="Create Job Cost Report Program"
00450   let rn$=lpad$(str$(rn),2)
00460 ! ______________________________________________________________________
00470   open #1: "Name=S:\acsPR\JCREPORT.MST,KFName=S:\acsPR\JCREPORT.idx",internal,input,keyed 
00480   read #1,using L490,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey XIT
00490 L490: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
00500   close #1: 
00510 ! ______________________________________________________________________
00550   open #11: "Name=PROC."&session$,display,output ioerr L570
00560   close #11,free: 
00570 L570: open #11: "Name=PROC."&session$&",SIZE=0,RecL=255",display,output 
00580   pr #11: "Clear"
00590   pr #11: "ProcErr Return"
00600   pr #11: "Load S:\acsPR\newJCRpt-Mod"
00610   pr #11: "00010 ! Replace S:\acsPR\JCPrnt";str$(rn) !:
        ! this program is dynamicaly created by S:\acsPR\jcRptS1 and !:
        ! uses S:\acsPR\newJCRpt-Mod as a base"
00620   pr #11: '00051 RN$="';rn$;'"'
00630   let pf$="19900 pr #255, USING 19910: "
00640   af$="19910 FORM SKIP 1"
00650   let gpf$="20140 pr #255, USING 20150: "
00660   let gaf$="20150 FORM SKIP 2,""Grand Totals"""
00670   let jpf$="20025 pr #255, USING 20026: "
00680   let jaf$="20026 FORM SKIP 1,""Job Totals"""
00690   let upf$="20000 pr #255,using 20020: "
00700   let uaf$="20020 form skip 0"
00710   pr #11: "19850 on zdiv goto 25000"
00720   pr #11: "19851 on uflow goto 25000"
00730   pr #11: "19852 on oflow goto 25000"
00740   pr #11: "25000 continue"
00750   pr #11: "25001 ! ______________________________________________________________________"
00760   pr #11: "25010 jn=0"
00770   pr #11: "25020 cn=0"
00780   pr #11: "25030 Continue"
00790   pr #11: "25031 ! ______________________________________________________________________"
00800   pr #11: '25050 cn$=""'
00810   pr #11: "25060 Continue"
00820   pr #11: "25061 ! ______________________________________________________________________"
00830   for j=1 to 20
00840     if rtrm$(f$(j))="" then goto L920
00850     for j2=1 to 30 ! Search for Column Within Formula
00860       if uprc$(f$(j)(j2:j2))="C" then goto L900
00870     next j2
00880     pr #11: str$(19850+j*2+1);" c(";str$(j);")=c(";str$(j);")+";f$(j)
00890     goto L910
00900 L900: pr #11: str$(19850+j*2+1);" c("&str$(j);")=";f$(j)
00910 L910: let j1=j1+1
00920 L920: next j
00930   pr #11: "19895 x6=0"
00940   pr #11: "19896 x7=0"
00950   pr #11: "19897 x8=0"
00960   pr #11: "19898 x9=0"
00970   pr #11: "19899 if sd = 1 then 19833"
00980   for j=1 to 20
00990     if rtrm$(f$(j))="" then goto L1350
01000     if f$(j)(3:3)>="0" and f$(j)(3:3)<="9" then goto L1060 !:
            ! Search Category Record
01010     if f$(j)(1:2)="x1" or f$(j)(1:2)="X1" then !:
            let i$="jn$(1:"&str$(ppr(j))&")"
01020     if f$(j)(1:2)="x2" or f$(j)(1:2)="X2" then !:
            let i$="n$(1:"&str$(ppr(j))&")"
01030     if f$(j)(1:2)="x3" or f$(j)(1:2)="X3" then !:
            let i$="a$(1)  (1:"&str$(ppr(j))&")"
01040     if f$(j)(1:2)="x4" or f$(j)(1:2)="X4" then !:
            let i$="a$(2)  (1:"&str$(ppr(j))&")"
01050     if f$(j)(1:2)="x5" or f$(j)(1:2)="X5" then !:
            let i$="a$(3)  (1:"&str$(ppr(j))&")"
01060 L1060: if f$(j)(1:3)="x10" or f$(j)(1:3)="X10" then !:
            let i$="cn$(7:11)"
01070     if f$(j)(1:3)="x11" or f$(j)(1:3)="X11" then !:
            let i$="k$(1:"&str$(ppr(j))&")"
01080     if rtrm$(i$)="" then cn=1 else cn=0
01090     if rtrm$(i$)="" then let i$="c("&str$(j)&")"
01100     if fc(j)=1 then goto L1170 ! Skip Detail Print
01110     let pf$=rtrm$(pf$)&","&i$ ! pr Statement
01120     if j=<1 then goto L1150
01130     if fc(j-1)=1 then goto L1150
01140     if pp(j)<pp(j-1)+ppr(j-1) then af$=af$&",skip 1"
01150 L1150: if cn=1 then !:
            af$=rtrm$(af$)&",POS "&str$(pp(j))&",N "&str$(ppr(j)) !:
            ! Form Statement
01160     if cn<>1 then !:
            af$=rtrm$(af$)&",pos "&str$(pp(j))&",C "&str$(ppr(j)) !:
            ! Form Statement
01170 L1170: if tcs(j)=0 then goto L1210
01180     let i$(1:1)="t"
01190     let gpf$=rtrm$(gpf$)&","&i$ ! pr Stmt-Grand Totals
01200     let gaf$=rtrm$(gaf$)&",pos "&str$(pp(j))&",N "&str$(ppr(j)) !:
          ! Form Statement Grand Totals
01210 L1210: if tcj(j)=0 then goto L1270
01220     let i$(1:1)="s"
01230     let jpf$=rtrm$(jpf$)&","&i$ ! pr Stmt-Job Totals
01240     let jaf$=rtrm$(jaf$)&",pos "&str$(pp(j))&",N "&str$(ppr(j)) !:
          ! Form Statement Job Totals
01250     let upf$=rtrm$(upf$)&","""&underlin$(1:ppr(j))&""""
01260     let uaf$=rtrm$(uaf$)&",pos "&str$(pp(j))&",C "&str$(ppr(j)) !:
          ! Underline Form Statement
01270 L1270: if dp(j)=0 then goto L1340
01280     if fc(j)=1 then goto L1300
01290     af$=rtrm$(af$)&"."&str$(dp(j)) ! Add Decimal Points
01300 L1300: if tcs(j)=0 then goto L1320
01310     let gaf$=rtrm$(gaf$)&"."&str$(dp(j)) !:
          ! Add Decimal Points-Grand Totals
01320 L1320: if tcj(j)=0 then goto L1340
01330     let jaf$=rtrm$(jaf$)&"."&str$(dp(j)) ! ADD DECIMAL POINTS-JOB TOTALS
01340 L1340: let i$=" "
01350 L1350: next j
01360   let pf$(31:31)=" "
01370   let pf$=rtrm$(pf$)&" pageoflow 300"
01380   pr #11: pf$
01390   af$(11:11)=" "
01400   af$=rtrm$(af$)&",skip 0"
01410   pr #11: af$
01420   pr #11: '19911 IF FILE$(255)(1:4)<>"PRN:" THEN pr #255:'
01430   pr #11: "19920 mat t=t+c"
01440   pr #11: "19930 mat s=s+c"
01450   pr #11: "19940 mat c=(0)"
01460   pr #11: "19941 jn$="""""
01470   pr #11: "19942 n$="""""
01480   pr #11: "19945 if sd><0 then 19800"
01490   pr #11: "19950 IF CN$(1:6)=JN1$ and sd=0 then 19833"
01500   if rtrm$(gpf$(31:255))="" then goto L1560
01510   let gpf$(31:31)=" "
01520   let gaf$=rtrm$(gaf$)&",skip 1"
01530   pr #11: gpf$
01540   pr #11: gaf$
01550   goto L1570
01560 L1560: pr #11: "20105 goto 390"
01570 L1570: if rtrm$(jpf$(31:255))="" then goto L1700
01580   let jpf$(31:31)=" "
01590   let jaf$=rtrm$(jaf$)&",skip 1"
01600   pr #11: jpf$
01610   pr #11: jaf$
01620   let upf$(30:30)=" "
01630   let uaf$(11:11)=" "
01640   pr #11: upf$
01650   let uaf$=rtrm$(uaf$)&",skip 0"
01660   pr #11: uaf$
01670   pr #11: "20030 mat s=(0)"
01680   pr #11: "20040 goto 19800"
01690   goto L1710
01700 L1700: pr #11: "20000 goto 19800"
01710 L1710: pr #11: "20100 snd: !"
01720   pr #11: "20110 pr #255: newpage"
01730   pr #11: "20120 gosub hdr"
01740   pr #11: "20160 goto 390"
01750   if ips=0 then goto L2130
01760   if ips>9 then goto L1950
01770   on sc goto L1880,L1790,L1820,L1850 none L1880
01780 ! ______________________________________________________________________
01790 L1790: pr #11: "19814 if ";ty$(ips);">=psc(1) then 19820 else 19800"
01800   goto L2130
01810 ! ______________________________________________________________________
01820 L1820: pr #11: "19814 if ";ty$(ips);"<=psc(1) then 19820 else 19800"
01830   goto L2130
01840 ! ______________________________________________________________________
01850 L1850: pr #11: "19814 if ";ty$(ips);">=psc(1) and ";ty$(ips);"<=psc(2) then 19820 else 19800"
01860   goto L2130
01870 ! ______________________________________________________________________
01880 L1880: pr #11: "19811 for j=1 to 100"
01890   pr #11: "19814   IF "&ty$(ips)&"= PSC(J) then 19820"
01900   pr #11: "19813   IF PSC(J)=0 then 19800"
01910   pr #11: "19815 next j"
01920   pr #11: "19816 goto 19800"
01930   goto L2130
01940 ! ______________________________________________________________________
01950 L1950: on sc goto L2060,L1970,L2000,L2030 none L2060
01960 ! ______________________________________________________________________
01970 L1970: pr #11: "19837 if ";ty$(ips);">=psc(1) then 19850 else 19833"
01980   goto L2130
01990 ! ______________________________________________________________________
02000 L2000: pr #11: "19837 if ";ty$(ips);"<=psc(1) then 19850 else 19833"
02010   goto L2130
02020 ! ______________________________________________________________________
02030 L2030: pr #11: "19837 if ";ty$(ips);">=psc(1) and ";ty$(ips);"<=psc(2) then 19850 else 19833"
02040   goto L2130
02050 ! ______________________________________________________________________
02060 L2060: pr #11: "19837 for j=1 to 100"
02070   pr #11: "19839   IF ";ty$(ips);"= PSC(J) then 19850"
02080   pr #11: "19838   IF PSC(J)=0 then 19833"
02090   pr #11: "19840 next j"
02100   pr #11: "19841 goto 19833"
02110   goto L2130
02120 ! ______________________________________________________________________
02130 L2130: pr #11: "Free S:\acsPR\jcPrnt"&str$(rn)&".br -n"
02140   pr #11: "Save S:\acsPR\jcPrnt"&str$(rn)
02150   pr #11: "run S:\acsPR\newjcrptfm"
02160   close #11: 
02170   chain "Proc=Proc."&session$
02180 ! ______________________________________________________________________
02190 ! <Updateable Region: ERTN>
02200 ERTN: let fnerror(program$,err,line,act$,"xit")
02210   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02240 ERTN_EXEC_ACT: execute act$ : goto ERTN
02250 ! /region
02260 ! ______________________________________________________________________
02270 XIT: let fnxit
02280 ! ______________________________________________________________________
