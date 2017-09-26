00010 ! formerly S:\acsUB\ubRate
00020 ! -- Rate File editor
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnflexinit1,fnflexadd1,fnacs,fnlbl,fntxt,fnmsgbox,fnopenprn,fncloseprn,fncomboa,fnopt,fntos,fnerror,fnxit,fncmdset,fntop,fncmdkey,fnget_services
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k$*25,k$(20)*25,rt$(35)*50,option$(10),msgline$(5)*40,snm$(10)*20
00080   dim item$(4)*30,resp$(40)*50,resp$*50,resp$(35)*50
00090 ! ______________________________________________________________________
10020   let fntop(program$)
10080   fnget_services(mat snm$,mat srv$)
10140   let x=0
10160   for j=1 to 10
10180     if trim$(snm$(j))<>"" then let option$(x+=1)=srv$(j)
10200   next j
10220   mat option$(x)
10240   open #1: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Use,RecL=374,KPs=1,KLn=4,Shr",internal,outin,keyed 
10250   open #2: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx2.h"&env$('cno')&",Use,RecL=374,KPs=5,KLn=25,Shr",internal,outin,keyed 
10260   goto SCREEN_GRID ! program starts with flex grid of all rates currently in file
20000 SCREEN_GRID: ! r:
20020   let fntos(sn$="rateflex")
20040   let myline=1 : let mypos=1 : let height=10 : let width=50
20060   let colhdr$(1)="Code"
20080   let colhdr$(2)="Description"
20100   let colhdr$(3)="Minimum"
20120   let colhdr$(4)="1st Rate"
20140   mat colhdr$(4)
20160   let colmask$(1)=colmask$(2)=""
20180   let colmask$(3)="10"
20200   let colmask$(4)="36"
20220   mat colmask(4)
20240   let fnflexinit1("ubrate",myline,mypos,height,width,mat colhdr$,mat colmask$,1)
20260   restore #1: 
20280   do 
20300     read #1,using 'Form POS 1,C 2,G 2,C 50,32*G 10': mat rt$ eof L1010
20320     let item$(1)=rt$(1)&rt$(2)
20340     let item$(2)=rt$(3)(1:30)
20360     let item$(3)=rt$(4)
20380     let item$(4)=rt$(8)
20400     let fnflexadd1(mat item$)
20420   loop 
20440 L1010: ! 
20460   let fnlbl(11,50,"",0,0)
20480   let fncmdkey("Edit",1,1,0,"Allows you to access the record that is highlited")
20500   let fncmdkey("&Add",2,0,0,"Add new rates")
20520   let fncmdkey("&Delete",4,0,0,"Deletes highlighted record")
20540   let fncmdkey("&Print",3,0,0,"Prints rate file proof list")
20560   let fncmdkey("&Complete",5,0,1,"Return to menu")
20580   let fnacs(sn$,0,mat resp$,ckey) ! CALL FLEXGRID
21000   let k$=rpad$(resp$(1),4)
21020   if ckey=5 then 
21040     goto XIT
21060   else if ckey=2 then 
21080     goto ADDNEWRECORD
21100   else if ckey=3 then 
21120     gosub PRINTPROOF
21140   else if ckey=1 then 
21160     goto RATEMAINT
21180   else if ckey=4 then 
21200     gosub DELETEREC
21220   end if 
21240   goto SCREEN_GRID
21260   goto XIT
21280 ! /r
30000 DELETEREC: ! r:
30020   mat msgline$(2)
30040   let msgline$(1)="Delete this rate record?"
30060   let msgline$(2)=k$ ! rt$(1)&rt$(2)
30080   let fnmsgbox(mat msgline$,resp$,'',36)
30120   if uprc$(resp$)(1:1)="Y" then 
30140     delete #1,key=k$: ! rt$(1)&rt$(2):
30160   end if 
30180   return  ! /r
40000 ADDNEWRECORD: ! r:
40010   mat rt$=("")
40020   let fntos(sn$="rateadd")
40040   mat resp$=("")
40060   let fnlbl(1,1,"Service Type:",20,1)
40080   let fnlbl(1,29,"Rate Code:",10,1)
40100   let fncomboa("rate_type",1,22,mat option$,"All codes must be between 1 and 99",2)
40120   let resp$(1)=""
40140   let fntxt(1,40,2,0,0,"",0,"All codes must be between 1 and 99")
40160   let fncmdset(2)
40180   let fnacs(sn$,0,mat resp$,ckey) ! CALL ADD NEW RECORD
40200   if ckey=5 then goto SCREEN_GRID
40220   let rt$=uprc$(resp$(1)) ! service type
40240   if rtrm$(rt$)="" then 
40260     mat msgline$(1)
40280     let msgline$(1)="Invalid Service Type"
40300     let fnmsgbox(mat msgline$,resp$,'',16)
40320     goto ADDNEWRECORD
40340   end if 
40360 ! 
40380   let g1=0 : let g1=val(resp$(2)) conv ignore ! rate code
40420   if g1=0 then 
40440     mat msgline$(1)
40460     let msgline$(1)="Rate codes must be from 1 to 99!"
40480     let fnmsgbox(mat msgline$,resp$,'',16)
40500     goto ADDNEWRECORD
40520   end if 
40540 ! 
40560   let rt$=rt$
40580   mat rt$=("")
40600   let rt$(1)=rt$
40620   for j=1 to udim(option$)
40640     if rt$(1)=option$(j) then goto ANR_SERVICE_TYPE_IS_VALID
40660   next j
40680   mat msgline$(1)
40700   let msgline$(1)="Invalid Service Type"
40720   let fnmsgbox(mat msgline$,resp$,'',16)
40740   goto ADDNEWRECORD
40760 ANR_SERVICE_TYPE_IS_VALID: ! 
40780 ! 
40800   let rt$(2)=lpad$(str$(g1),2)
40820   let k$=rt$(1)&rt$(2)
40840   read #1,using 'Form POS 1,C 2,G 2,C 50,32*G 10',key=k$: mat rt$ nokey L470
40860   goto RATEMAINT ! existing record
40880 L470: ! 
40900   write #1,using 'Form POS 1,C 2,G 2,C 50,32*G 10': mat rt$
40920   goto RATEMAINT ! create new rate record
40940 ! /r
50000 RATEMAINT: ! r: maintain rate file
50010   read #1,using 'Form POS 1,C 2,G 2,C 50,32*G 10',key=k$: mat rt$ nokey ignore
50040   let fntos(sn$="ratemaint")
50060   let c1=20 : let c2=32 : let c3=44
50080   let fnlbl(1,1,"Service Type:",20,1)
50100 ! let fncomboa("ubrate3",1,22,mat option$,"All codes must be between 1 and 99",2)
50120   let fntxt(1,22,2,0,0,"",1)
50140   let fnlbl(1,29,"Rate Code:",10,1)
50160   let fntxt(1,40,2,0,0,"30",1)
50180 ! 
50200   let fnlbl(2,1,"Description:",20,1)
50220   let fnlbl(3,1,"Minimum Charge:",20,1)
50240   let fnlbl(3,30,"Minimum Usage:",20,1)
50260   let fntxt(2,22,50)
50280   let fntxt(3,22,9,0,1,"32")
50300 ! if env$('client')="Franklinton" and rt$(1)="GA" then 
50320 !   let fntxt(3,51,9,0,1,"31")
50340 ! else 
50360     let fntxt(3,51,9,0,1,"30")
50380 ! end if 
50400   let fnlbl(5,c1,"Usage",9,2)
50420   let fnlbl(5,c2,"Usage",9,2)
50440   let fnlbl(5,c3," Charge",9,2)
50460   let fnlbl(6,c1," From",9,2)
50480   let fnlbl(6,c2," To",9,2)
50500   let fnlbl(6,c3,"Per Unit",9,2)
50520   let x=7
50540   for lin=6 to 15
50560     ! if env$('client')="Franklinton" and rt$(1)="GA" then ! Special Franklinton routines
50580     !   let fntxt(lin+1,c1,9,9,1,"31",0) ! 1 decimal
50600     !   let fntxt(lin+1,c2,9,9,1,"31",0)
50620     ! else 
50640       let fntxt(lin+1,c1,9,10,1,"30",0) ! 0 decimal
50660       let fntxt(lin+1,c2,9,10,1,"30",0)
50680     ! end if 
50700     let fntxt(lin+1,c3,9,9,1,"36")
50720     let x=x+3
50740   next lin
50760   let fncmdset(4)
50762   mat resp$(udim(mat rt$))
50764   mat resp$=rt$
50780   let fnacs(sn$,0,mat resp$,ckey) !        ! CALLS RATE MAINTENANCE
50782   mat resp$(udim(mat rt$))
50784   mat rt$=resp$
50800   if ckey=5 then goto SCREEN_GRID
60000 ! 
60020   let rt$(1)=uprc$(rt$(1)) ! service type
60040   if rtrm$(rt$(1))="" then 
60060     mat msgline$(1)
60080     let msgline$(1)="Invalid Service Type: "&rt$(1)
60100     let fnmsgbox(mat msgline$,resp$,'',16)
60120     goto RATEMAINT
60140   end if 
60160 ! 
60180   let g1=0 : let g1=val(rt$(2)) conv ignore ! rate code
60200   if g1=0 then 
60220     mat msgline$(1)
60240     let msgline$(1)="Rate codes must be from 1 to 99!"
60260     let fnmsgbox(mat msgline$,resp$,'',48)
60280     goto RATEMAINT
60300   end if 
60320 ! 
60340   for j=1 to udim(option$)
60360     if rt$(1)=uprc$(option$(j)) then goto RM_SERVICE_TYPE_VALID
60380   next j
60400   mat msgline$(1)
60420   let msgline$(1)="Invalid Service Type: "&rt$(1)
60440   let fnmsgbox(mat msgline$,resp$,'',16)
60460 RM_SERVICE_TYPE_VALID: ! 
60480 ! 
60500   let rt$(2)=lpad$(str$(g1),2)
60520   let k$=rt$(1)&rt$(2)
60560   rewrite #1,using 'Form POS 1,C 2,G 2,C 50,32*G 10',key=k$: mat rt$
60580   goto SCREEN_GRID
60600 ! /r
70000 PRINTPROOF: ! r:
70020   let fntos(sn$="RateProof")
70040   let fnopt(1,14,"Code Sequence")
70060   let resp$(1)="True"
70080   let fnopt(2,14,"Name Sequence")
70100   let resp$(2)="False"
70120   let fncmdset(2)
70140   let fnacs(sn$,0,mat resp$,ckey) ! CALLS PROOF LIST
70160   if ckey=5 then goto SCREEN_GRID
70180   let ti2=1 ! default to code sequence
70200   if uprc$(resp$(1))=uprc$("True") then let ti2=1: let k$="    " ! code sequence
70220   if uprc$(resp$(2))=uprc$("True") then let ti2=2: let k$=rpt$(chr$(0),25) ! name sequence
70240   restore #ti2,key>=k$: ! Nokey SCREEN_GRID
70260   let fnopenprn
70280   let pg=0
70300   do 
70320     read #ti2,using 'Form POS 1,C 2,G 2,C 50,32*G 10',release: mat rt$ eof PRINT_PROOF_FINIS
70340     gosub PRINT_ONE_RATE
70360   loop 
70380 PRINT_PROOF_FINIS: ! 
70400   let fncloseprn
70420   return  ! /r
70440 PRINT_ONE_RATE: ! r:
70460   print #255: "Service Code: ";rt$(1);"   Rate Code: ";rt$(2)
70480   print #255: "Description: ";rt$(3)
70500   print #255: "Minimum Charge: ";ltrm$(rt$(4));"   Minimum Usage: ";ltrm$(rt$(5))
70520   print #255: ""
70540   print #255: "Usage From     Usage To    Rate Per Unit"
70560   print #255: "__________    __________   _____________"
70580   for j=1 to udim(option$)
70600     if trim$(rt$(j*3+3))<>"" or trim$(rt$(j*3+4))<>"" or trim$(rt$(j*3+5))<>"" then 
70620       print #255,using 'Form POS 1,3*C 14': rt$(j*3+3),rt$(j*3+4),rt$(j*3+5)
70640     end if 
70660   next j
70680   print #255: ""
70700   print #255: ""
70720   if pg+=1<3 then goto P1R_FINIS
70740   print #255: newpage
70760   let pg=0
70780 P1R_FINIS: ! 
70800   return  ! /r
80000 XIT: let fnxit
80020 IGNORE: continue 
80040 ! <Updateable Region: ERTN>
80060 ERTN: let fnerror(program$,err,line,act$,"xit")
80080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80120   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
80140 ERTN_EXEC_ACT: execute act$ : goto ERTN
80160 ! /region
