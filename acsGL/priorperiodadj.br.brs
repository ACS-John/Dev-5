00010 ! Replace S:\acsGL\PriorPeriodAdj
00020 ! -- Enter Prior Period Adjustments
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnerror,fncno,fndat, fnTos,fnLbl,fnqgl,fnCmdSet,fnAcs,fnagl$,fnTxt,fncombof,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim d$*50,bc(13),bp(13),scr$(8)*20
00080   dim cnam$*40,dat$*20,fm(4),cap$*128,resp$(10)*80
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Prior Period Adjustments")
00110   fnconsole(off=0)
00120   fncno(cno,cnam$) !:
        fndat(dat$)
00130   right=1 : center=2 : limit_to_list=1
00140   ac1=1
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative: read #20,using "Form pos 384,N 2",rec=1: nap !:
        close #20: 
00160   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLINDEX.h"&env$('cno')&",Shr",internal,outIn,keyed 
00170   open #2: "Name="&env$('Q')&"\GLmstr\ACTRANS.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\ACTRIDX.h"&env$('cno')&",Shr",internal,outIn,keyed 
00175   fnopenprn
00180 MENU1: ! 
00185   resp$(2)=""
00190   fnTos(sn$='Prior_Period_Adj') !:
        lc=0 !:
        mylen=40 : mypos=mylen+2
00200   fnLbl(lc+=1,1,"General Ledger Number:",mylen,right)
00210   fnqgl(lc,mypos,0,2) !:
        resp$(1)=""
00220   fnLbl(lc+=1,1,"Adjustment Amount:",mylen,right)
00230   fnTxt(lc,mypos,12,0,0,'pointtwo')
00240   fnLbl(lc+=1,1,"Date:",mylen,right)
00250   fnTxt(lc,mypos,0,0,0,'1')
00260   lc+=1
00270   fnLbl(lc+=1,1,"First Period Affected:",mylen,right)
00280   fncombof('Period',lc,mypos,0,env$('Q')&"\GLmstr\Period.h"&env$('cno'),1,2,3,25,env$('Q')&"\GLmstr\Period-Idx.h"&env$('cno'),limit_to_list)
00290   fnLbl(lc+=1,1,"First Year Affected:",mylen,right)
00300   fncombof('Year',lc,mypos,0,env$('Q')&"\GLmstr\Year.h"&env$('cno'),1,1,2,7,env$('Q')&"\GLmstr\Year-Idx.h"&env$('cno'),limit_to_list)
00310   lc+=1
00320   fnLbl(lc+=1,1,"Last Period Affected:",mylen,right)
00330   fncombof('Period',lc,mypos,0,env$('Q')&"\GLmstr\Period.h"&env$('cno'),1,2,3,25,env$('Q')&"\GLmstr\Period-Idx.h"&env$('cno'),limit_to_list)
00340   fnLbl(lc+=1,1,"Last Year Affected:",mylen,right)
00350   fncombof('Year',lc,mypos,0,env$('Q')&"\GLmstr\Year.h"&env$('cno'),1,1,2,7,env$('Q')&"\GLmstr\Year-Idx.h"&env$('cno'),limit_to_list)
00360   fnCmdSet(2)
00370   fnAcs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto L690
00390   k$=fnagl$(resp$(1)) !:
        am=val(resp$(2)) !:
        d1=val(resp$(3)) !:
        fm(1)=val(resp$(4)(1:2)) !:
        fm(2)=val(resp$(5)(1:2)) !:
        fm(3)=val(resp$(6)(1:2)) !:
        fm(4)=val(resp$(7)(1:2))
00395   if val(k$)=0 and am=0 then goto XIT
00400   if fm(2)=1 then fm2$="C" else fm2$="P"
00410   if fm(4)=1 then fm4$="C" else fm4$="P"
00420 ! 
00440   gosub HDR
00450   read #1,using 'Form POS 13,C 50,POS 81,41*PD 6.2',key=k$: d$,bb,cb,mat bc,mat bp
00460   ce=0
00470   pr #255,using 'Form POS 9,PIC(ZZZ),X 6,PIC(ZZZZZZ),X 9,PIC(ZZZ),X 4,C 35,X 1,N 2,X 1,C 1,X 11,N 2,X 1,C 1,X 4,N 11.2': val(k$(1:3)),val(k$(4:9)),val(k$(10:12)),d$(1:35),fm(1),fm2$,fm(3),fm4$,am pageoflow PGOF
00480   if am>0 then am1=am1+am else am2=am2+am
00490   goto L500
00500 L500: if fm(2)=1 then goto L580
00510   if fm(4)=1 then last=nap else last=fm(3)
00520   for j=fm(1) to last
00530     bp(j)=bp(j)+am
00540   next j
00550   if fm(4)=2 then goto L640
00560   first=1
00570   goto L590
00580 L580: first=fm(1)
00590 L590: for j=first to fm(3)
00600     bc(j)=bc(j)+am
00610   next j
00620   bb=bb+am
00630   cb=cb+am
00640 L640: rewrite #1,using 'Form POS 81,41*PD 6.2',key=k$: bb,cb,mat bc,mat bp
00650   if ac1=0 then goto L680
00660   if fm(2)><1 then goto L680 ! CURRENT YEAR ONLY
00670   write #2,using 'Form POS 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': k$,d1,am,3,0,"PPAJ"&date$,"Prior Period Adjustment",fm(1)
00680 L680: goto MENU1
00690 L690: close #1: 
00700   close #2: 
00710   pr #255,using 'Form POS 5,C 18,N 12.2': "Total Debits: ",am1
00720   pr #255,using 'Form POS 5,C 18,N 12.2': "Total Credits: ",am2
00730   pr #255,using 'Form POS 5,C 18,N 12.2': "Net Adjustments: ",am1+am2
00740   fncloseprn
00750   execute "Index "&env$('Q')&"\GLmstr\AcTrans.h"&env$('cno')&' '&env$('Q')&"\GLmstr\AcTrIdx.h"&env$('cno')&" 1/71/17/13 12/2/2/4 Replace DupKeys"
00760   goto XIT
00770 ! ______________________________________________________________________
00780 HDR: ! 
00790   pr #255,using 'Form Pos 20,Cc 40': cnam$ !:
        pr #255,using 'Form Pos 20,Cc 40': cap$ !:
        pr #255,using 'Form Pos 20,Cc 40': dat$ !:
        pr #255: ""
00800   pr #255,using 'Form POS 4,C 12,X 2,C 12,C 13,POS 43,C 12,POS 73,C 60': scr$(1),scr$(2),scr$(3),"Description","1st Month/Yr   Last Month/Yr   Amount"
00810   return 
00820 ! ______________________________________________________________________
00830 PGOF: ! 
00840   pr #255: newpage
00850   gosub HDR
00860   continue 
00870 ! ______________________________________________________________________
00880 ! <updateable region: ertn>
00890 ERTN: fnerror(program$,err,line,act$,"xit")
00900   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00910   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00920   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00930 ERTN_EXEC_ACT: execute act$ : goto ERTN
00940 ! /region
00950 ! ______________________________________________________________________
00960 XIT: fncloseprn: fnxit
00970 ! ______________________________________________________________________
