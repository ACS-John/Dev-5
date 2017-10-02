10040 ! r: setup
10060   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnerror,fntos,fncmdset,fngethandle,fnopenfile,fnFree
10080   on error goto ERTN
10100 ! ______________________________________________________________________
10110   dim Comp$(0)*128,CompN(0),form$(0)*256
10120   dim cap$*128
14000 ! /r
14060   let fntop(program$)
15000   gosub COMPANY_LOAD
18020   let fntos(sn$="Company") : col1Len=19 : col2Pos=col1Len+2
18030 ! r: company information portion of screen
18040   let fnlbl(1,1,"Name:",col1Len,1)
18060   let fntxt(1,col2Pos,40)
18100   let fnlbl(2,1,"Address:",col1Len,1)
18120   let fntxt(2,col2Pos,40)
18160   let fnlbl(3,1,"City,State and Zip:",col1Len,1)
18180   let fntxt(3,col2Pos,40)
23000 ! /r
40000   let fncmdset(2)
40020   let fnacs(sn$,0,mat Comp$,ck)
42000   if ck<>5 then 
54000     gosub COMPANY_SAVE
58000   end if
59000 XIT: let fnxit
59500 IGNORE: continue 
60640 ! <Updateable Region: ERTN>
60660 ERTN: let fnerror(program$,err,line,act$,"xit")
60680   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
60700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60720   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60740 ERTN_EXEC_ACT: execute act$ : goto ERTN
60760 ! /region
72000 COMPANY_LOAD: ! r:
72020   hCompany=fnopenfile(env$('cursys')&' Company',mat Comp$,mat CompN,mat form$, 0,0,0,unused$,mat unused$,mat unused,mat unused$,supressprompt:=2)
72040   read #hCompany,using form$(hCompany): mat Comp$,mat CompN ioerr ignore
72060   close #hCompany: ioerr ignore
72140 return  ! /r
74000 COMPANY_SAVE: ! r:
74020   fnFree(env$('Q')&'\'&env$('cursys')&'mstr\Company.h'&env$('cno'))
74070   dim fileiosubs$(0)*512
74080   hCompany=fnopenfile(env$('cursys')&' Company',mat Comp$,mat CompN,mat form$, 0,0,0,unused$,mat unused$,mat unused,mat fileiosubs$,supressprompt:=2)
74100   write #hCompany,using form$(hCompany): mat Comp$,mat CompN 
74120   close #hCompany: 
74140 return  ! /r

