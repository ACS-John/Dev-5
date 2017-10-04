00010 ! Replace S:\acsPR\JCCPR1
00020 ! Certified Payroll Register
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox,fnerror,fncno,fndate_mmddyy_to_ccyymmdd,fntop,fnxit,fnchain,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,cn$*11,ta(2),tr(9),io1$(2),n$*40,tn$*6,dr(7),en$*12
00080   dim cap$*128,message$*40,msgline$(2)*60,response$(5)*1
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\jcCPR1",cap$="Certified Payroll Register")
00110   fncno(cno)
00120 ! 
00125   fnconsole(1)
00130   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00140   open #2: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00150   open #3: "Name="&env$('Q')&"\PRmstr\JCTRANS.h"&str$(cno)&",Shr",internal,input,relative 
00160   open #4: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=55,Replace",internal,output 
00170 ! ______________________________________________________________________
00180   pr newpage
00190   fnopenwin(win=101,08,20,13,59,cap$)
00200   pr #win,fields "4,2,Cr 23,N": "Starting Date (mmddyy):"
00210   pr #win,fields "5,2,Cr 23,N": "Ending Date (mmddyy):"
00220   let io1$(1)="4,26,Nz 6,UT,N"
00230   let io1$(2)="5,26,Nz 6,UT,N"
00240   pr f "14,30,C 09,B,1": "Next (F1)"
00250   pr f "14,41,C 09,B,5": "Exit (F5)"
00260 L260: input #win,fields mat io1$: df,dt conv CONV1
00270   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
00280   if cmdkey>0 then goto L350 else ce=curfld
00290 L290: ce=ce+1: if ce>udim(io1$) then ce=1
00300 L300: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L290
00310   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L260
00320 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00330   ce=cnt+1
00340 ERR1: pr f "24,78,C 1": bell : goto L300
00350 L350: if cmdkey=5 then goto XIT
00360   if df<10100 or df>123199 then ce=1: goto ERR1
00370   if dt<10100 or dt>123199 then ce=2: goto ERR1
00380 ! ______________________________________________________________________
00390   mat dr=(0)
00400   let dr(1)=int(df*.01)
00410   let dr(7)=int(dt*.01)
00420   let df1=df
00430   let dt1=dt
00440   let df=fndate_mmddyy_to_ccyymmdd(df)
00450   let dt=fndate_mmddyy_to_ccyymmdd(dt)
00460   if dt<df then goto L260
00470   for j=2 to 7
00480     if dr(9-j)-int(dr(9-j)*.01)*100-1=0 then goto L510
00490     let dr(8-j)=dr(7)-j+1
00500   next j
00510 L510: if dr(1)=0 then let dr(1)=int(df1*.01)
00520   for j=2 to 6
00530     if dr(j)>0 then goto L560
00540     let dr(j)=dr(1)+j-1
00550   next j
00560 L560: write #4,using L570: df1,dt1,mat dr
00570 L570: form pos 1,2*n 6,7*pd 3
00580 L580: pr newpage
00590   fnopenwin(win=102,08,20,13,59,cap$)
00600   if rw>0 then !:
          pr #win,fields "6,1,Cc 40,R,N": "Last Job Number entered was "&ltrm$(jn$)
00610   pr #win,fields "4,2,C 20,N": "Job Number to Print:"
00620   pr f "14,29,C 10,B,2": "Print (F2)"
00630   pr f "14,40,C 11,B,5": "Cancel (F5)"
00640 L640: input #win,fields "4,23,C 6,UET,N": jn$
00650   let jn$=lpad$(rtrm$(jn$),6)
00660   if cmdkey=2 then goto L940
00670   if cmdkey=5 then goto XIT
00680   if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L640
00690   let rw=0
00700   cn$=jn$&"     "
00710   read #2,using L720,key>=cn$: cn$,mat ta nokey L740
00720 L720: form pos 1,c 11,pos 118,2*pd 3
00730   goto L800
00740 L740: if rw>0 then goto L580
00750   let msgline$(1)="No Transactions exist for Job Number "&ltrm$(jn$)
00760   let msgline$(2)="within the specified date range."
00770   fnoldmsgbox(mat response$,cap$,mat msgline$,1)
00780   goto L580
00790 L790: read #2,using L720: cn$,mat ta eof L740
00800 L800: if jn$><cn$(1:6) then goto L740
00810   if ta(1)=0 then goto L790
00820   adr=ta(1)
00830 L830: read #3,using L840,rec=adr: en$,tn$,mat tr,nta
00840 L840: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,x 30,pd 3
00850   if tr(5)+tr(6)=0 then goto L910
00860   let tr4=fndate_mmddyy_to_ccyymmdd(tr(4))
00870   if tr4<df or tr4>dt then goto L910
00880   write #4,using L890: en$,tn$,mat tr
00890 L890: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
00900   let rw=rw+1
00910 L910: if nta=0 then goto L790
00920   adr=nta
00930   goto L830
00940 L940: if rw=0 then goto XIT
00950   let message$="Sorting: please wait..."
00960   fnwait(103,cap$,message$,0)
00970   close #2: 
00980   close #3: 
00990   close #4: 
01000   open #1: "Name="&env$('Temp')&"\Control."&session$,internal,output 
01010   restore #1: 
01020 L1020: form pos 1,c 128
01030   write #1,using L1020: "FILE "&env$('Temp')&"\Work."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,acsPR,,A,N"
01040   write #1,using L1020: "MASK 13,6,c,a,1,12,c,a,33,2,c,a,29,4,c,a"
01050   close #1: 
01060   execute "FREE "&env$('Temp')&"\Addr."&session$&" -n"
01070   execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01080   fnchain("S:\acsPR\JCCPR2")
01090 ! ______________________________________________________________________
01100 XIT: let fnxit
01110 ! ______________________________________________________________________
01120 ! <Updateable Region: ERTN>
01130 ERTN: let fnerror(program$,err,line,act$,"xit")
01140   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01150   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01160   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01170 ERTN_EXEC_ACT: execute act$ : goto ERTN
01180 ! /region
01190 ! ______________________________________________________________________
