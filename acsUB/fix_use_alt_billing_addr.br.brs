00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fntop,fnChk,fnerror,fnTos,fncno,fnxit,fnCmdSet,fngethandle
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*80
00080   dim cap$*128
00090   dim z$*10,e$(4)*30,ba$(4)*30
00150 ! ______________________________________________________________________
20000   fntop(program$,cap$='Fix Use Alternate Billing Address')
20020   fncno(cno,cnam$)
30000 ! r: SCREEN1:
30020   fnTos(sn$="UBPrtBl1-1") 
30040   pf=34 : ll=32 : width=pf+8
30060   respc=0
30080   fnLbl(1,1,"Warning:  Continuing will change the Use/Do Not Use flag",width,2)
30100   fnLbl(2,1,"on all customer billing addresses. Anyone with a non-blank",width,2)
30120   fnLbl(3,1,"billing address will be changed to use that billing address.",width,2)
30140   fnLbl(7,1,"Date of Billing (blank for all):",ll,1)
30160   fnTxt(7,pf,8,8,1,"1") 
30180   resp$(1)='' ! cnvrt$("pic(zzzzzz)",d1)
30200   fnCmdSet(2) 
30220   fnAcs(sn$,0,mat resp$,ck)
30240   if ck=5 then goto XIT
30260   d1=val(resp$(1))
30280 ! /r
40000 ! r: main loop
40020   open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,outIn,keyed
40040   open #hAltBillAddr:=fngethandle: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
40060   changeCount=0
50000   do
50020     read #hCustomer,using 'form pos 1,c 10,4*c 30,pos 1854,pd 5.2': z$,mat e$,extra22 eof FINIS
50040     if ~f or f=d1 and extra22<=0 then
50060       read #hAltBillAddr,using 'form pos 11,4*c 30',key=z$: mat ba$ nokey abNokey
50080       if rtrm$(ba$(1)&ba$(2)&ba$(3)&ba$(4))<>"" then 
50100         extra22=1
50120         changeCount+=1
50140         rewrite #hCustomer,using 'form pos 1854,PD 5.2': extra22
50160       end if
50180     end if
50200   abNokey: !
50220   loop
50240 ! /r
60000 FINIS: ! r: pr totals screen
60020   close #hCustomer: ioerr ignore
60040   close #hAltBillAddr: ioerr ignore
60060   fnTos(sn$="Bills-Total") 
60080   mylen=53 : mypos=mylen+2 
60100   respc=0
60120   fnLbl(1,1,"Total Customers Set to use atlernate billing address:",mylen,1)
60140   fnTxt(1,mypos,8,0,1,"",1) 
60160   resp$(respc+=1)=str$(changeCount)
60180   fnCmdSet(52) 
60200   fnAcs(sn$,0,mat resp$,ck) ! /r
60220 XIT: fnxit
60240 ! ______________________________________________________________________
76020 ! <updateable region: ertn>
76040 ERTN: fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
