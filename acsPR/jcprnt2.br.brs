00010 ! Replace S:\acsPR\JCPrnt2
00020 ! newJCRpt-MOD will be modified by S:\acsPR\newjcRptS1 to make used designed JCPrntXX
00030 ! DO NOT RENUMBER !!!
00040 ! ______________________________________________________________________
00050   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenwin,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fndat,fntos,fnlbl,fntxt,fnacs,fncmdset
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
00090   dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
00100   dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,cnam$*40,jn1$*6
00110   dim cap$*128,message$*40
00120 ! ______________________________________________________________________
00130   fntop("S:\acsPR\newPrUsrDR",cap$="Print User Designed Reports (2)")
00140   fncno(cno,cnam$) !:
        fndat(dh$)
00150 ! 
00160 ! ______________________________________________________________________
00170   let rn$=" 2"
00180   if fnprocess=1 then goto L103
00190 ! ______________________________________________________________________
00200 MAIN_SCREEN: ! 
00210   fntos(sn$="namlst1") !:
        let mylen=25 : let mypos=mylen+2: let resp=0: let left=1
00220   fnlbl(1,1,"Report Heading Date:",23,left)
00230   fntxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") !:
        let resp$(resp+=1)=dh$
00240   fncmdset(2)
00250   fnacs(sn$,0,mat resp$,ck)
00260   if ck=5 then goto XIT
00270   let dat$=dh$=resp$(1) ! heading date
00280   close #win: ioerr L69
00285 L69: let fndat(dh$,put=2)
00290 ! ______________________________________________________________________
00295   fndat(dh$,2)
00300 ! ______________________________________________________________________
00302 L103: let fnopenprn
00304 ! ______________________________________________________________________
00306   open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",internal,input,keyed 
00308   read #1,using L170,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
00310 L170: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
00312   close #1: 
00314 ! ______________________________________________________________________
00316   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00318   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00320   gosub HDR
00322   goto PRTRPT
00324 ! ______________________________________________________________________
00326 PGOF: pr #255: newpage : gosub HDR : continue 
00328 ! ______________________________________________________________________
00330 HDR: ! 
00332   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00334   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00336   pr #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
00338   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dh$)&"}"
00340 ! pr #255: "\qc  {\f181 \fs16 \b "&TRIM$(D$)&"}"
00342   pr #255: "\ql   "
00344   pr #255: ch$(1)
00346   pr #255: ch$(2)
00348   return 
00350 ! ______________________________________________________________________
00352 EOF1: ! 
00354   fncloseprn
00356   close #1: 
00358   close #2: 
00360   fnxit
00362 ! ______________________________________________________________________
00364 PRTRPT: read #1,using L19810: jn$,n$,mat a$,x6,x7,x8,x9 eof SND
00366   let jn1$=jn$
00368   on conv goto L25010
00370   let jn=val(jn$)
19810 L19810: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
19820   if sd=2 then goto L19900
19830   let jobcat$=jn$&"     "
19840   read #2,using L19831,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
19845 L19831: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
19850   goto L19834
19852 L19833: read #2,using L19831: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L25050
19854 L19834: cn=val(cn$(7:11))
19856   if cn$(1:6)><jn1$ and sd=1 then goto L19900
19858   if cn$(1:6)><jn1$ and sd=0 then goto L20000
19860 ! ______________________________________________________________________
19862   on zdiv goto L25000
19864   on uflow goto L25000
19866   on oflow goto L25000
19868   c(1)=c(1)+x1
19870   c(2)=c(2)+x2
19872   c(3)=c(3)+x12+x14
19874   c(4)=c(4)+((x12*x23)+(x14*x24))/100
19876   c(5)=(c(4)/c(3))*100
19878   c(6)=c(6)+x15+x17
19880   c(7)=c(7)+x15
19882   c(8)=c(8)+x17
19884   c(9)=((c(6)/c(5))*100)
19886   let x6=0
19888   let x7=0
19890   let x8=0
19892   let x9=0
19894   if sd = 1 then goto L19833
19900 L19900: pr #255, using L19910: jn$(1:6),n$(1:25),c(3),c(4),c(5),c(6),c(7),c(8),c(9) pageoflow PGOF
19910 L19910: form skip 1,pos 1,c 6,pos 9,c 25,pos 35,n 11.2,pos 50,n 10.2,pos 63,n 3,pos 67,n 11.2,pos 80,n 11.2,pos 92,n 11.2,pos 110,n 11.2,skip 0
19920   if file$(255)(1:4)<>"PRN:" then pr #255: 
19930   mat t=t+c
19940   mat s=s+c
19950   mat c=(0)
19960   let jn$=""
19970   let n$=""
19980   if sd><0 then goto PRTRPT
19990   if cn$(1:6)=jn1$ and sd=0 then goto L19833
20000 L20000: goto PRTRPT
20010 SND: ! 
20020   pr #255: newpage
20030   gosub HDR
20040   pr #255, using L20150: t(3),t(4),t(6),t(7),t(8),t(9)
20150 L20150: form skip 2,"Grand Totals",pos 35,n 11.2,pos 50,n 10.2,pos 67,n 11.2,pos 80,n 11.2,pos 92,n 11.2,pos 110,n 11.2,skip 1
20160   goto EOF1
25000 L25000: continue 
25010 ! ______________________________________________________________________
25015 L25010: let jn=0
25020   cn=0
25025   continue 
25030 ! ______________________________________________________________________
25050 L25050: cn$=""
25060   continue 
25070 ! ______________________________________________________________________
25080 ! ______________________________________________________________________
25090 ! <Updateable Region: ERTN>
25100 ERTN: let fnerror(program$,err,line,act$,"xit")
25110   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
25120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
25130   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
25140 ERTN_EXEC_ACT: execute act$ : goto ERTN
25150 ! /region
25160 ! ______________________________________________________________________
25170 XIT: let fnxit
25180 ! ______________________________________________________________________
