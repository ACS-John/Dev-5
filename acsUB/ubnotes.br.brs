00030   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fncmbrt2,fnTos,fnerror,fnwait,fnopenprn,fncloseprn,fnxit,fnLastBillingDate,fndat,fnCmdSet,fntop,fngethandle
00040   on error goto Ertn
00050   dim cap$*128
00060 ! 
00070   dim e$(4)*30
00080   dim cap$*128,resp$(2)*40,txt$*40,a$*1,line$*90
00090 ! 
00120   fntop(program$,cap$="Notes")
00130   fnLastBillingDate(d1)
00180 SCR1: ! 
00190   fnTos(sn$="ubnotes") 
00192   respc=0
00200   fnLbl(1,1,"Route Number:",31,1)
00210   fncmbrt2(1,33) 
00212   resp$(respc+=1)="1"
00220   fnLbl(2,1,"Billing Date (Blank for all):",31,1)
00230   fnTxt(2,33,8,0,0,"1") 
00232   resp$(respc+=1)=str$(d1)
00240   fnCmdSet(3) 
00242   fnAcs(sn$,0,mat resp$,ckey)
00250   if ckey=5 then goto XIT
00260   if resp$(1)="[All]" then rt1=0 else rt1=val(resp$(1))
00270   d1=val(resp$(2))
00280 ! 
00290   open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed 
00300   fnopenprn
00310   gosub HEADER
00320 ! 
00330   if rt1 then
00333     bk$=lpad$(str$(rt1),2)
00340     restore #h_customer,key>=bk$&"       ": nokey SCR1
00342   end if 
00350 READ_CUSTOMER: !
00352   read #h_customer,using 'form pos 1,c 10,4*c 30,pos 296,pd 4,pos 1741,n 2,n 7': z$,mat e$,f,route,sequence eof DONE
00370   if rt1>0 and route<>rt1 then goto DONE
00380   if d1<>0 and f><d1 then goto READ_CUSTOMER
00390 ! READ_NOTE: ! 
00400   close #31: ioerr ignore
00410   open #31: "Name=[Q]\UBmstr\notes.h[cno]\"&trim$(z$)&".txt",display,input ioerr READ_CUSTOMER
00420   pr #255: "" 
00422   pr #255: "{\b "&rpad$(trim$(z$),10)&"  Street Adr: "&e$(1)&"  Name: "&e$(2)&"}"
00430   do
00432     linput #31: line$ eof READ_CUSTOMER
00440     pr #255: line$ pageoflow PGOF
00450   loop
00460 ! 
00470 DONE: ! 
00480   fncloseprn
00490 XIT: fnxit
00500 ignore: continue
00510 ! <Updateable Region: ERTN>
00520 ERTN: fnerror(program$,err,line,act$,"xit")
00530   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00540   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00550   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00560 ERTN_EXEC_ACT: execute act$ : goto ERTN
00570 ! /region
00590 HEADER: ! r:
00600   p2=p2+1
00610   pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
00620   pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
00630   pr #255: "\qc  {\f181 \fs18 \b "&date$("Month DD, CCYY")&"}"
00640   pr #255,using 'form pos 1,c 82,c 10': "\ql "&date$,"Page "&str$(p2)
00660 return ! /r
00670 PGOF: ! r:
00672   pr #255: newpage
00680   gosub HEADER
00690 continue ! /r
00700 ! 
