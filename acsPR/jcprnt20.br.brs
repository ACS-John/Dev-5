00010 ! Replace S:\acsPR\JCPrnt20
00011 ! newJCRpt-MOD will be modified by S:\acsPR\newjcRptS1 to make used designed JCPrntXX
00012 ! DO NOT RENUMBER !!!
00015 ! ______________________________________________________________________
00016   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenwin,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fndat,fntos,fnlbl,fntxt,fnacs,fncmdset
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
00051   let rn$="20"
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
00300 PGOF: pr #255: newpage : gosub HDR : continue 
00321 ! ______________________________________________________________________
00330 HDR: ! 
00331   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00332   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00333   pr #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
00334   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dh$)&"}"
00335 ! pr #255: "\qc  {\f181 \fs16 \b "&TRIM$(D$)&"}"
00336   pr #255: "\ql   "
00360   pr #255: ch$(1)
00370   pr #255: ch$(2)
00380   return 
00381 ! ______________________________________________________________________
00390 EOF1: ! 
00400   let fncloseprn
00410   close #1: 
00420   close #2: 
00430   let fnxit
19799 ! ______________________________________________________________________
19800 PRTRPT: read #1,using L19810: jn$,n$,mat a$,x6,x7,x8,x9 eof SND
19805   let jn1$=jn$
19806   on conv goto L25010
19807   let jn=val(jn$)
19810 L19810: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
19820   if sd=2 then goto L19900
19825   let jobcat$=jn$&"     "
19830   read #2,using L19831,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
19831 L19831: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
19832   goto L19834
19833 L19833: read #2,using L19831: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L25050
19834 L19834: cn=val(cn$(7:11))
19835   if cn$(1:6)><jn1$ and sd=1 then goto L19900
19836   if cn$(1:6)><jn1$ and sd=0 then goto L20000
19849 ! ______________________________________________________________________
19850   on zdiv goto L25000
19851   on uflow goto L25000
19852   on oflow goto L25000
19853   c(1)=c(1)+x1
19855   c(2)=c(2)+x2
19857   c(3)=c(3)+x10
19859   c(4)=c(4)+x11
19861   c(5)=c(5)+x22
19863   c(6)=c(6)+x21
19865   c(7)=c(7)+x12+x14
19867   c(8)=c(8)+x15+x17
19869   c(9)=c(9)+(x12+x14)/x22
19871   c(10)=c(10)+(x15+x17)/x21
19873   c(11)=c(11)+(x12+x14)*x23/100
19875   c(12)=c(12)+(x21/x22)*((x12+x14)*x23/100)
19877   c(13)=c(11)-c(7)
19879   c(14)=c(12)-c(8)
19881   c(15)=(c(12)/c(8))*100
19895   let x6=0
19896   let x7=0
19897   let x8=0
19898   let x9=0
19899   if sd = 1 then goto L19833
19900 L19900: pr #255, using L19910: jn$(1:6),n$(1:23),cn$(7:11),k$(1:16),c(5),c(6),c(7),c(8),c(9),c(10),c(11),c(12),c(13),c(14),c(15) pageoflow PGOF
19910 L19910: form skip 1,pos 1,c 6,pos 9,c 23,skip 1,pos 13,c 11,pos 26,c 16,pos 42,n 5,pos 48,n 5,pos 55,n 8.2,pos 65,n 8.2,pos 74,n 6.2,pos 82,n 6.2,pos 90,n 8.2,pos 100,n 8.2,pos 110,n 8.2,pos 120,n 8.2,pos 129,n 4,skip 0
19911   if file$(255)(1:4)<>"PRN:" then pr #255: 
19920   mat t=t+c
19930   mat s=s+c
19940   mat c=(0)
19941   let jn$=""
19942   let n$=""
19945   if sd><0 then goto PRTRPT
19950   if cn$(1:6)=jn1$ and sd=0 then goto L19833
20000 L20000: pr #255,using L20020: "________","________","______","________","________","________","________"
20020 L20020: form skip 0,pos 55,c 8,pos 65,c 8,pos 74,c 6,pos 90,c 8,pos 100,c 8,pos 110,c 8,pos 120,c 8,skip 0
20025   pr #255, using L20026: s(7),s(8),s(9),s(11),s(12),s(13),s(14)
20026 L20026: form skip 1,"Job Totals",pos 55,n 8.2,pos 65,n 8.2,pos 74,n 6.2,pos 90,n 8.2,pos 100,n 8.2,pos 110,n 8.2,pos 120,n 8.2,skip 1
20030   mat s=(0)
20040   goto PRTRPT
20100 SND: ! 
20110   pr #255: newpage
20120   gosub HDR
20140   pr #255, using L20150: t(7),t(8),t(9),t(11),t(12),t(13),t(14)
20150 L20150: form skip 2,"Grand Totals",pos 55,n 8.2,pos 65,n 8.2,pos 74,n 6.2,pos 90,n 8.2,pos 100,n 8.2,pos 110,n 8.2,pos 120,n 8.2,skip 1
20160   goto EOF1
25000 L25000: continue 
25001 ! ______________________________________________________________________
25010 L25010: let jn=0
25020   cn=0
25030   continue 
25031 ! ______________________________________________________________________
25050 L25050: cn$=""
25060   continue 
25061 ! ______________________________________________________________________
49849 ! ______________________________________________________________________
50000 ! <Updateable Region: ERTN>
50001 ERTN: let fnerror(program$,err,line,act$,"xit")
50002   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
50003   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50004   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50005 ERTN_EXEC_ACT: execute act$ : goto ERTN
50006 ! /region
50007 ! ______________________________________________________________________
50120 XIT: let fnxit
50121 ! ______________________________________________________________________
