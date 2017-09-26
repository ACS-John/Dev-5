00010 ! Replace S:\acsPR\newjcRpt-MOD
00011 ! newJCRpt-MOD will be modified by S:\acsPR\newjcRptS1 to make used designed JCPrntXX
00012 ! DO NOT RENUMBER !!!
00015 ! ______________________________________________________________________
00016   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fndat,fntos,fnlbl,fntxt,fnacs,fncmdset
00017   on error goto ERTN
00020 ! ______________________________________________________________________
00021   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
00023   dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
00024   dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,cnam$*40,jn1$*6
00025   dim cap$*128,message$*40
00030 ! ______________________________________________________________________
00031   let fntop("S:\acsPR\newPrUsrDR",cap$="Print User Designed Reports (2)")
00032   let fncno(cno,cnam$) !:
        let fndat(dh$)
00038 ! 
00040 ! ______________________________________________________________________
00055   if fnprocess=1 then goto L103
00056 ! ______________________________________________________________________
00059 MAIN_SCREEN: ! 
00060   let fntos(sn$="namlst1") !:
        let mylen=25 : let mypos=mylen+2: let resp=0: let left=1
00062   let fnlbl(1,1,"Report Heading Date:",23,left)
00063   let fntxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") !:
        let resp$(resp+=1)=dh$
00064   let fncmdset(2)
00065   let fnacs(sn$,0,mat resp$,ck)
00066   if ck=5 then goto XIT
00067   let dat$=dh$=resp$(1) ! heading date
00068   close #win: ioerr L69
00069 L69: let fndat(dh$,put=2)
00075 ! ______________________________________________________________________
00080   let fndat(dh$,2)
00088 ! ______________________________________________________________________
00103 L103: let fnopenprn
00141 ! ______________________________________________________________________
00150   open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",internal,input,keyed 
00160   read #1,using L170,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
00170 L170: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
00180   close #1: 
00181 ! ______________________________________________________________________
00260   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00270   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00280   gosub HDR
00290   goto PRTRPT
00291 ! ______________________________________________________________________
00300 PGOF: print #255: newpage : gosub HDR : continue 
00321 ! ______________________________________________________________________
00330 HDR: ! 
00331   print #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00332   print #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00333   print #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
00334   print #255: "\qc  {\f181 \fs16 \b "&trim$(dh$)&"}"
00335 ! Print #255: "\qc  {\f181 \fs16 \b "&TRIM$(D$)&"}"
00336   print #255: "\ql   "
00360   print #255: ch$(1)
00370   print #255: ch$(2)
00380   return 
00381 ! ______________________________________________________________________
00390 EOF1: ! 
00400   let fncloseprn
00410   close #1: 
00420   close #2: 
00430   let fnxit
19799 ! ______________________________________________________________________
19800 PRTRPT: read #1,using L19810: jn$,n$,mat a$,x6,x7,x8,x9 eof L20100
19805   let jn1$=jn$
19806   on conv goto L25010
19807   let jn=val(jn$)
19810 L19810: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
19820   if sd=2 then goto L19900
19825   let jobcat$=jn$&"     "
19830   read #2,using L19831,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
19831 L19831: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
19832   goto L19834
19833   read #2,using L19831: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L25050
19834 L19834: let cn=val(cn$(7:11))
19835   if cn$(1:6)><jn1$ and sd=1 then goto L19900
19836   if cn$(1:6)><jn1$ and sd=0 then goto L20000
19849 ! ______________________________________________________________________
19850 ! this section of lines will be replaced during dynamics
19900 L19900: ! this section of lines will be replaced during dynamics
20000 L20000: ! this section of lines will be replaced during dynamics
20100 L20100: ! this section of lines will be replaced during dynamics
25010 L25010: ! this section of lines will be replaced during dynamics
25050 L25050: ! this section of lines will be replaced during dynamics
49849 ! ______________________________________________________________________
50000 ! <Updateable Region: ERTN>
50001 ERTN: let fnerror(program$,err,line,act$,"xit")
50002   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
50003   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50004   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
50005 ERTN_EXEC_ACT: execute act$ : goto ERTN
50006 ! /region
50007 ! ______________________________________________________________________
50120 XIT: let fnxit
50121 ! ______________________________________________________________________
