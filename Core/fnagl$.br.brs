00010 ! Replace S:\Core\fnAGL$
00020 ! format the answer to fnQgl -
00030   def library fnagl$*12(&x$)
00040     library 'S:\Core\Library': fngethandle,fnerror,fnpause
00050     on error goto Ertn
00060 !_
00070     if x$="[All]" or x$='' then let fnagl$="  0     0  0" : goto XIT
00080     tmp_cursys$=env$('CurSys')
00090     if env$('CurSys')='UB' and exists("[Q]\GLmstr\Company.h[cno]") then tmp_cursys$="GL": goto COMPANY_OPEN
00100     if env$('CurSys')='UB' and exists("[Q]\UBmstr\GLmstr.h[cno]") then tmp_cursys$="UB": goto COMPANY_POST_READ
00110     if env$('CurSys')='PR' and exists("[Q]\GLmstr\Company.h[cno]") then tmp_cursys$="GL": goto COMPANY_OPEN
00120     if env$('CurSys')='PR' and exists("[Q]\CLmstr\Company.h[cno]") then tmp_cursys$="CL": goto COMPANY_OPEN
00130     if env$('CurSys')='PR' and exists("[Q]\PRmstr\glmstr.h[cno]") then tmp_cursys$="PR": goto COMPANY_OPEN
00140     if env$('CurSys')='CR' and exists("[Q]\GLmstr\GLmstr.h[cno]") then tmp_cursys$="GL": goto COMPANY_OPEN
00150     if env$('CurSys')='CR' and exists("[Q]\GLmstr\GLmstr.h[cno]")=0 then tmp_cursys$="CR": goto COMPANY_OPEN
00160     if env$('CurSys')='CL' then tmp_cursys$='CL' else tmp_cursys$='GL'
00170 ! find out if I should use the department number and/or the sub account number
00180 COMPANY_OPEN: ! 
00190     open #company:=fngethandle: "Name=[Q]\"&tmp_cursys$&"mstr\Company.h[cno],Shr",internal,input ioerr COMPANY_OPEN_IOERR
00200     if tmp_cursys$='GL' then 
00210       read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub
00220     else if tmp_cursys$='CL' then 
00230       read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub
00240     end if 
00250 COMPANY_POST_READ: ! 
00260     if tmp_cursys$="PR" or tmp_cursys$="UB" or tmp_cursys$="CR" then use_dept=use_sub=1
00270     close #company: ioerr ignore
00275 ! strip off any description
00280     x$=x$(1:14)
00285 ! find the position of the "-"s
00290     dash1=pos(x$,"-")
00295     dash2=pos(x$,"-",-1)
00300 ! reformat it into a read key= ready format
00301     if dash1=0 and dash2=0 and len(x$)=12 then 
00302 ! do nothing - it is already formatted properly
00305     else if use_dept<>0 and use_sub<>0 then 
00310       x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3)
00315     else if use_dept =0 and use_sub<>0 then 
00320       x$="  0"&lpad$(trim$(x$(1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3)
00325     else if use_dept =0 and use_sub =0 then 
00330       x$="  0"&lpad$(trim$(x$),6)&"  0"
00335     else if use_dept<>0 and use_sub =0 then 
00340       x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:len(x$))),6)&"  0"
00345     end if 
00350 ! If USE_DEPT =0 AND USE_SUB =0 Then Goto 350 Else Goto 370
00355 ! If X$(1:3)="  0" Then x$(1:3)="   " ! kj
00360 ! If X$(10:12)="  0" Then x$(10:12)="   " ! kj
00365 ! x$="  0"&LPAD$(TRIM$(X$),6)&"  0": Goto 390 ! kj
00368 FINIS: ! 
00370     x$=lpad$(trim$(x$),12)
00432     fnagl$=x$(1:12)
00440     goto XIT
00450 COMPANY_OPEN_IOERR: ! r:
00452     x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3) ! default if gl or cl not installed
00460     goto FINIS ! /r
00470 !_
00480 ! <Updateable Region: ERTN>
00490 ERTN: fnerror(program$,err,line,act$,"xit")
00500     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00510     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00520     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00530 ERTN_EXEC_ACT: execute act$ : goto ERTN
00540 ! /region
00550 IGNORE: continue 
00560 XIT: fnend 
00570 !_
