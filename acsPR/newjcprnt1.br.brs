00010 ! Replace S:\acsPR\newJCPrnt1
00020 ! JCRpt-MOD will be modified by S:\acsPR\jcRptS1 to make used designed JCPrntXX
00030 ! DO NOT RENUMBER !!!
00040 ! ______________________________________________________________________
00050   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenwin,fnopenprn,fncloseprn,fnerror,fnprocess,fndat,fntos,fnlbl,fntxt,fncmdset,fnacs
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
00090   dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
00100   dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,jn1$*6
00110   dim message$*40
00120 ! ______________________________________________________________________
00130 ! fnTOP("S:\acsPR\jcRpt-MOD",CAP$="User Designed Reports (2)")
00140   fndat(dh$)
00150 ! 
00160 ! ______________________________________________________________________
00170   rn$=" 1"
00180   if fnprocess=1 then goto L300
00190 ! ______________________________________________________________________
00200 MAIN_SCREEN: ! 
00210   fntos(sn$="jcprnt1") !:
        mylen=25 : mypos=mylen+2: resp=0: left=1
00220   fnlbl(1,1,"Report Heading Date:",23,left)
00230   fntxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") !:
        resp$(resp+=1)=dh$
00240   fncmdset(2)
00250   fnacs(sn$,0,mat resp$,ck)
00260   if ck=5 then goto XIT
00270   dh$=resp$(1) ! heading date
00280   fndat(dh$,put=2)
00290 ! ______________________________________________________________________
00300 L300: fnopenprn
00310 ! ______________________________________________________________________
00330 ! ______________________________________________________________________
00340   open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",internal,input,keyed 
00350   read #1,using L360,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
00360 L360: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
00370   close #1: 
00380 ! ______________________________________________________________________
00390   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00400   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00410   gosub HDR
00420   goto PRTRPT
00430 ! ______________________________________________________________________
00440 PGOF: pr #255: newpage : gosub HDR : continue 
00450 ! ______________________________________________________________________
00460 HDR: ! r:
00470   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00480   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00490   pr #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
00500   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dh$)&"}"
00510   pr #255: "\ql   "
00520   pr #255: ch$(1)
00530   pr #255: ch$(2)
00540   return ! /r
00550 ! ______________________________________________________________________
00560 EOF1: ! 
00570   fncloseprn
00580   close #1: 
00590   close #2: 
00600   fnxit
00610 ! ______________________________________________________________________
00620 PRTRPT: read #1,using L660: jn$,n$,mat a$,x6,x7,x8,x9 eof SND
00630   jn1$=jn$
00640   on conv goto L1240
00650   jn=val(jn$)
00660 L660: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
00670   if sd=2 then goto L1000
00680   jobcat$=jn$&"     "
00690   read #2,using L700,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
00700 L700: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
00710   goto L730
00720 L720: read #2,using L700: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L1280
00730 L730: cn=val(cn$(7:11))
00740   if cn$(1:6)><jn1$ and sd=1 then goto L1000
00750   if cn$(1:6)><jn1$ and sd=0 then goto L1100
00760   for j=1 to 100
00770     if psc(j)=0 then goto L720
00780     if x12= psc(j) then goto L820
00790   next j
00800   goto L720
00810 ! ______________________________________________________________________
00820 L820: on zdiv goto L1220
00830   on uflow goto L1220
00840   on oflow goto L1220
00850   c(1)=c(1)+x1
00860   c(2)=c(2)+x2
00870   c(3)=c(3)+x10
00880   c(4)=c(4)+x11
00890   c(5)=c(5)+x23
00900   c(7)=c(7)+x21
00910   c(8)=c(8)+x12/x22
00920   c(9)=c(9)+x15/x21
00930   c(10)=c(10)+((x15/x21)-(x12/x22))*x21
00940   c(11)=c(10)/x23*100
00950   let x6=0
00960   let x7=0
00970   let x8=0
00980   let x9=0
00990   if sd = 1 then goto L720
01000 L1000: pr #255, using L1010: jn$(1:8),n$(1:23),cn$(7:11),k$(1:18),c(5),c(7),c(8),c(9),c(10),c(11) pageoflow PGOF
01010 L1010: form skip 1,pos 1,c 8,pos 9,c 23,pos 33,c 11,pos 46,c 18,pos 64,n 3,pos 79,n 8.2,pos 88,n 6.2,pos 96,n 6.2,pos 106,n 8.2,pos 119,n 8.2,skip 0
01020   if file$(255)(1:4)<>"PRN:" then pr #255: 
01030   mat t=t+c
01040   mat s=s+c
01050   mat c=(0)
01060   jn$=""
01070   n$=""
01080   if sd><0 then goto PRTRPT
01090   if cn$(1:6)=jn1$ and sd=0 then goto L720
01100 L1100: pr #255,using L1110: "________","________","________"
01110 L1110: form skip 0,pos 79,c 8,pos 106,c 8,pos 119,c 8,skip 0
01120   pr #255, using L1130: s(7),s(10),s(11)
01130 L1130: form skip 1,"Job Totals",pos 79,n 8.2,pos 106,n 8.2,pos 119,n 8.2,skip 1
01140   mat s=(0)
01150   goto PRTRPT
01160 SND: ! 
01170   pr #255: newpage
01180   gosub HDR
01190   pr #255, using L1200: t(7),t(10),t(11)
01200 L1200: form skip 2,"Grand Totals",pos 79,n 8.2,pos 106,n 8.2,pos 119,n 8.2,skip 1
01210   goto EOF1
01220 L1220: continue 
01230 ! ______________________________________________________________________
01240 L1240: jn=0
01250   cn=0
01260   continue 
01270 ! ______________________________________________________________________
01280 L1280: cn$=""
01290   continue 
01300 ! ______________________________________________________________________
01310 ! ______________________________________________________________________
01320 ! <Updateable Region: ERTN>
01330 ERTN: fnerror(program$,err,line,act$,"xit")
01340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01370 ERTN_EXEC_ACT: execute act$ : goto ERTN
01380 ! /region
01390 ! ______________________________________________________________________
01400 XIT: fnxit
01410 ! ______________________________________________________________________
