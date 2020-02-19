00010 ! Replace S:\acsPR\JCPrnt7
00020 ! newJCRpt-MOD will be modified by S:\acsPR\newjcRptS1 to make used designed JCPrntXX
00030 ! DO NOT RENUMBER !!!
00040 !
00050   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fndat,fnTos,fnLbl,fnTxt,fnAcs,fnCmdSet
00060   on error goto Ertn
00070 !
00080   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
00090   dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
00100   dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,cnam$*40,jn1$*6
00110   dim cap$*128,message$*40
00120 !
00130   fntop("S:\acsPR\newPrUsrDR",cap$="Print User Designed Reports (2)")
00140   fncno(cno,cnam$) !:
        fndat(dh$)
00150 ! 
00160 !
00170   rn$=" 7"
00180   if fnprocess=1 then goto L103
00190 !
00200 MAIN_SCREEN: ! 
00210   fnTos(sn$="namlst1") !:
        mylen=25 : mypos=mylen+2: resp=0: left=1
00220   fnLbl(1,1,"Report Heading Date:",23,left)
00230   fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") !:
        resp$(resp+=1)=dh$
00240   fnCmdSet(2)
00250   fnAcs(sn$,0,mat resp$,ck)
00260   if ck=5 then goto XIT
00270   dat$=dh$=resp$(1) ! heading date
00280   close #win: ioerr L69
00285 L69: fndat(dh$,put=2)
00290 !
00295   fndat(dh$,2)
00300 !
00302 L103: fnopenprn
00304 !
00306   open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",internal,input,keyed 
00308   read #1,using L170,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
00310 L170: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
00312   close #1: 
00314 !
00316   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
00318   open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
00320   gosub HDR
00322   goto PRTRPT
00324 !
00326 PGOF: pr #255: newpage : gosub HDR : continue 
00328 !
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
00350 !
00352 EOF1: ! 
00354   fncloseprn
00356   close #1: 
00358   close #2: 
00360   fnxit
00362 !
00364 PRTRPT: read #1,using L19810: jn$,n$,mat a$,x6,x7,x8,x9 eof SND
00366   jn1$=jn$
00368   on conv goto L25010
00370   jn=val(jn$)
19810 L19810: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
19820   if sd=2 then goto L19900
19830   jobcat$=jn$&"     "
19840   read #2,using L19831,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
19845 L19831: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
19850   goto L19834
19852 L19833: read #2,using L19831: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L25050
19854 L19834: cn=val(cn$(7:11))
19856   if cn$(1:6)><jn1$ and sd=1 then goto L19900
19858   if cn$(1:6)><jn1$ and sd=0 then goto L20000
19860 !
19862   on zdiv goto L25000
19864   on uflow goto L25000
19866   on oflow goto L25000
19868   c(1)=c(1)+x1
19870   c(2)=c(2)+x2
19872   c(3)=c(3)+x10
19874   c(4)=c(4)+x11
19876   c(5)=c(5)+x22
19878   c(6)=c(6)+x21
19880   c(7)=c(7)+x23
19882   c(8)=c(8)+x12
19884   c(9)=c(9)+x13
19886   c(10)=c(10)+x15
19888   c(11)=c(11)+x16
19890   c(12)=c(12)+x12*x23/100
19892   c(13)=c(13)+x13*x23/100
19894   c(14)=c(14)+max((x12-x15),((x15/x23/100)-x15))
19896   c(15)=c(15)+max((x13-x16),((x16/x23/100)-x16))
19898   x6=0
19900   x7=0
19902   x8=0
19904   x9=0
19906   if sd = 1 then goto L19833
19908 L19900: pr #255, using L19910: jn$(1:6),n$(1:40),cn$(7:11),k$(1:19),c(5),c(6),c(7),c(8),c(9),c(10),c(11),c(12),c(13),c(14),c(15) pageoflow PGOF
19910 L19910: form skip 1,pos 5,c 6,pos 16,c 40,skip 1,pos 1,c 5,pos 11,c 19,pos 31,n 7,pos 43,n 7,pos 53,n 3,pos 58,n 7,pos 66,n 7,pos 74,n 7,pos 82,n 7,pos 93,n 5,pos 101,n 5,pos 107,n 7,pos 115,n 7,skip 0
19920   if file$(255)(1:4)<>"PRN:" then pr #255: 
19930   mat t=t+c
19940   mat s=s+c
19950   mat c=(0)
19960   jn$=""
19970   n$=""
19980   if sd><0 then goto PRTRPT
19990   if cn$(1:6)=jn1$ and sd=0 then goto L19833
20000 L20000: pr #255,using L20020: "_______","_______","_______","_______","_______","_______","_____","_____","_______","_______"
20020 L20020: form skip 0,pos 31,c 7,pos 43,c 7,pos 58,c 7,pos 66,c 7,pos 74,c 7,pos 82,c 7,pos 93,c 5,pos 101,c 5,pos 107,c 7,pos 115,c 7,skip 0
20030   pr #255, using L20026: s(5),s(6),s(8),s(9),s(10),s(11),s(12),s(13),s(14),s(15)
20035 L20026: form skip 1,"Job Totals",pos 31,n 7,pos 43,n 7,pos 58,n 7,pos 66,n 7,pos 74,n 7,pos 82,n 7,pos 93,n 5,pos 101,n 5,pos 107,n 7,pos 115,n 7,skip 1
20040   mat s=(0)
20045   goto PRTRPT
20050 SND: ! 
20055   pr #255: newpage
20060   gosub HDR
20065   pr #255, using L20150: t(5),t(6),t(8),t(9),t(10),t(11),t(12),t(13),t(14),t(15)
20150 L20150: form skip 2,"Grand Totals",pos 31,n 7,pos 43,n 7,pos 58,n 7,pos 66,n 7,pos 74,n 7,pos 82,n 7,pos 93,n 5,pos 101,n 5,pos 107,n 7,pos 115,n 7,skip 1
20160   goto EOF1
25000 L25000: continue 
25010 !
25015 L25010: jn=0
25020   cn=0
25025   continue 
25030 !
25050 L25050: cn$=""
25060   continue 
25070 !
25080 !
25090 ! <Updateable Region: ERTN>
25100 ERTN: fnerror(program$,err,line,act$,"xit")
25110   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
25120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
25130   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
25140 ERTN_EXEC_ACT: execute act$ : goto ERTN
25150 ! /region
25160 !
25170 XIT: fnxit
25180 !
