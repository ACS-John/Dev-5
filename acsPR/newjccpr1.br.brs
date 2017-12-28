00010 ! Replace S:\acsPR\newJCCPR1
00020 ! Certified Payroll Register
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fncno,fndate_mmddyy_to_ccyymmdd,fnchain,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fncmbjob,fnmsgbox,fnCmdKey
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,cn$*11,ta(2),tr(9),io1$(2),n$*40,tn$*6,dr(7),en$*12
00080   dim cap$*128,resp$(3)*50,ml$(2)*60
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Certified Payroll Register")
00110   fncno(cno)
00120 ! 
00130   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00140   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00150   open #3: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&env$('cno')&",Shr",internal,input,relative 
00160   open #4: "Name="&env$('temp')&"\Work."&session$&",SIZE=0,RecL=55,Replace",internal,output 
00170 ! ______________________________________________________________________
00180 MENU1: ! 
00190   fnTos(sn$="jccpr1") !:
        respc=0
00200   fnLbl(1,47," ",1,1)
00210   fnLbl(1,1,"Beginning Date:",20,1)
00220   fnTxt(1,23,8,0,0,"1",0,"First day of week being printed.") !:
        resp$(respc+=1)=str$(df)
00230   fnLbl(2,1,"Ending Date:",20,1)
00240   fnTxt(2,23,8,0,0,"1",0,"Week ending date.") !:
        resp$(respc+=1)=str$(dt)
00250   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ck)
00255   if ck=5 then goto XIT
00260   df=val(resp$(1)) ! beginning date
00270   dt=val(resp$(2)) ! ending date
00280 ! ______________________________________________________________________
00290   mat dr=(0)
00300   dr(1)=int(df*.01)
00310   dr(7)=int(dt*.01)
00320   df1=df
00330   dt1=dt
00340   df=fndate_mmddyy_to_ccyymmdd(df)
00350   dt=fndate_mmddyy_to_ccyymmdd(dt)
00360   if dt<df then goto MENU1
00370   for j=2 to 7
00380     if dr(9-j)-int(dr(9-j)*.01)*100-1=0 then goto L410
00390     dr(8-j)=dr(7)-j+1
00400   next j
00410 L410: if dr(1)=0 then dr(1)=int(df1*.01)
00420   for j=2 to 6
00430     if dr(j)>0 then goto L460
00440     dr(j)=dr(1)+j-1
00450   next j
00460 L460: write #4,using L470: df1,dt1,mat dr
00470 L470: form pos 1,2*n 6,7*pd 3
00480   goto ASKJOB
00490 ! ______________________________________________________________________
00500 ASKJOB: ! 
00510   fnTos(sn$="jccpr1J") !:
        respc=0
00520   fnLbl(1,1,"Job #:",8,1)
00530   fncmbjob(1,11) !:
        resp$(respc+=1)=jn$
00540   if trim$(jn$)<>"" then let fnLbl(3,1,"Last job processed:"&trim$(jn$),35,1)
00550   fnCmdKey("&Next",1,1,0,"Process the job" ) !:
        fnCmdKey("&Complete",2,0,0,"Start printing")
00560   fnAcs(sn$,0,mat resp$,ck)
00570   if ck=2 then goto PRINT_REPORT
00580   jn$=lpad$(trim$(resp$(1)(1:6)),6)
00590   rw=0
00600   cn$=jn$&"     "
00610   read #2,using L620,key>=cn$: cn$,mat ta nokey L640
00620 L620: form pos 1,c 11,pos 118,2*pd 3
00630   goto L670
00640 L640: if rw>0 then goto ASKJOB
00650   mat ml$(2) !:
        ml$(1)="No Transactions exist for Job Number "&ltrm$(jn$) !:
        ml$(2)="within the specified date range." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto ASKJOB
00660 L660: read #2,using L620: cn$,mat ta eof L640
00670 L670: if jn$><cn$(1:6) then goto L640
00680   if ta(1)=0 then goto L660
00690   adr=ta(1)
00700 L700: read #3,using L710,rec=adr: en$,tn$,mat tr,nta
00710 L710: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,x 30,pd 3
00720   if tr(5)+tr(6)=0 then goto L780
00730   tr4=fndate_mmddyy_to_ccyymmdd(tr(4))
00740   if tr4<df or tr4>dt then goto L780
00750   write #4,using L760: en$,tn$,mat tr
00760 L760: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
00770   rw=rw+1
00780 L780: if nta=0 then goto L660
00790   adr=nta
00800   goto L700
00810 PRINT_REPORT: ! 
00820   if rw=0 then goto XIT
00830   close #2: 
00840   close #3: 
00850   close #4: 
00860   open #1: "Name="&env$('Temp')&"\Control."&session$,internal,output 
00870   restore #1: 
00880 L880: form pos 1,c 128
00890   write #1,using L880: "FILE "&env$('temp')&"\Work."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,acsPR,,A,N"
00900   write #1,using L880: "MASK 13,6,c,a,1,12,c,a,33,2,c,a,29,4,c,a"
00910   close #1: 
00920   execute "FREE "&env$('Temp')&"\Addr."&session$&" -n"
00930   execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
00940   fnchain ("S:\acsPR\newJCCPR2")
00950 ! ______________________________________________________________________
00960 XIT: fnxit
00970 ! ______________________________________________________________________
00980 ! <Updateable Region: ERTN>
00990 ERTN: fnerror(program$,err,line,act$,"xit")
01000   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01010   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01020   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01030 ERTN_EXEC_ACT: execute act$ : goto ERTN
01040 ! /region
01050 ! ______________________________________________________________________
