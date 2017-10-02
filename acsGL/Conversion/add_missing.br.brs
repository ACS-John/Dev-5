00040   library 'S:\Core\Library': fntop,fnwait,fnopenprn,fncloseprn,fncno,fnerror,fnchain,fnprocess,fntop,fnxit,fndate_mmddyy_to_ccyymmdd,fnconsole,fngetdir2,fnputcno,fngethandle
00050   on error goto ERTN
00060 ! r: dims
00070   dim a$*40,em$*30,ta(2),cp(32),tcp(22),hc(5),thc(5),d$*20,whc(10)
00080   dim dedcode(10),calcode(10),dedfed(10),message$*40,cnam$*40,cap$*128
00110 ! /r
00120   let fntop("S:\acsGL\conversion\v4_part2",cap$="add missing files and indexes")
00130   let fncno(cno,cnam$)
00140 ! 
00170 ! r: main loop
02000   cno_origional=cno
02020   dim company_file$(1)*128
02040   let tmp$=env$('Q')&"\GLmstr"
02060   let fngetdir2(tmp$,mat company_file$, option$,'Company.h*')
02080   for company_item=1 to udim(mat company_file$)
02100     cno_current=val(company_file$(company_item)(10:len(company_file$(company_item))))
02120     if cno_current<>0 then 
02140       let fnputcno(cno_current) : cno=cno_current : ! 
02160 ! 
02180       open #paymstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\PayMstr.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\GLmstr\PayIdx1.h"&str$(cno)&",RecL=276,kln=8,kps=1,replace",internal,outin,keyed 
02200       close #paymstr: 
02220       execute "Index "&env$('Q')&"\GLmstr\paymstr.H"&str$(cno)&' '&env$('Q')&"\GLmstr\Payidx1.H"&str$(cno)&" 1 8 Replace DupKeys -N"
02240       execute "Index "&env$('Q')&"\GLmstr\paymstr.H"&str$(cno)&' '&env$('Q')&"\GLmstr\Payidx2.H"&str$(cno)&" 9 38 Replace DupKeys -N"
02260 ! 
02280       open #6: "Name="&env$('Q')&"\GLmstr\bankrec.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\bankrec-idx.H"&str$(cno)&",Version=1,Shr",internal,outin,keyed 
02300       close #6: 
02320       execute "Index "&env$('Q')&"\GLmstr\bankrec.H"&str$(cno)&' '&env$('Q')&"\GLmstr\bankrec-idx.h"&str$(cno) &" 79/3/4 12/1/8 Replace,DupKeys"
02340 ! 
02342       open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",RecL=64,Use",internal,outin 
02344       close #2: 
02348       execute "Index "&env$('Q')&"\GLmstr\gltr1099.H"&str$(cno)&' '&env$('Q')&"\GLmstr\gltridx1.H"&str$(cno)&" 1 8 Replace DupKeys -N"
02360     end if  ! cno_current<>0
02380   next company_item
02400   let fnputcno(cno_origional)
02420   goto XIT ! /r
03410 ! <Updateable Region: ERTN>
03420 ERTN: let fnerror(program$,err,line,act$,"xit")
03430   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03460 ERTN_EXEC_ACT: execute act$ : goto ERTN
03470 ! /region
03490 XIT: let fnxit
03500 ! ______________________________________________________________________
