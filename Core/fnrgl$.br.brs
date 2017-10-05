20000 ! Replace S:\Core\fnRgl$
20020 ! format the answer to fnQgl -
28000   def library fnrgl$*60(x$; ReturnMaxLength)
28020     library 'S:\Core\Library': fngethandle,fnerror,fnpause
28040     on error goto ERTN
28060     if ReturnMaxLength=0 then returnMaxLength=35
28080 ! _______________________________________________________________________
28100 ! X$ should be formatted as though it were just read in and is ready 
28120 !    for a read Key=...   ie "  0   100  0"
28140 ! _______________________________________________________________________
28160     dim desc$*50
28180 ! _______________________________________________________________________
28200     if env$('CurSys')="UB" and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno')) then cursys$="GL": goto L180
28220     if env$('CurSys')="UB" and exists(env$('Q')&"\UBmstr\GLmstr.h"&env$('cno')) then cursys$="UB": goto L180
28240     if env$('CurSys')="PR" and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno')) then cursys$="GL": goto L180
28260     if env$('CurSys')="PR" and exists(env$('Q')&"\CLmstr\Company.h"&env$('cno')) then cursys$="CL": goto L180
28280     if env$('CurSys')="PR" and exists(env$('Q')&"\PRmstr\glmstr.h"&env$('cno')) then cursys$="PR": goto L180
28300     if env$('CurSys')='CL' then cursys$='CL' else cursys$='GL'
28320     if env$('CurSys')='CR' and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno')) then cursys$='GL'
28340     if env$('CurSys')='CR' and exists(env$('Q')&"\GLmstr\Company.h"&env$('cno'))=0 then cursys$='CR'
28360 ! find out if I should use the department number and/or the sub account number
28380 L180: !
28400     if cursys$='GL' or cursys$='CL' then 
28420       open #company:=fngethandle: "Name="&env$('Q')&"\"&cursys$&"mstr\Company.h"&env$('cno')&",Shr",internal,input ioerr L260
28440       read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub
28460       close #company: 
28480     else if cursys$="PR" or cursys$="UB" or cursys$="CR" then 
28500       L260: !
28520       let use_dept=use_sub=1 ! default both to use
28540     end if
28560     desc$=''
28580     open #glmstr:=fngethandle: "Name="&env$('Q')&"\"&cursys$&"mstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\"&cursys$&"mstr\GLIndex.h"&env$('cno')&",Shr",internal,input,keyed ioerr L_ERR_OPEN_FOR_DESC
28600     read #glmstr,using "Form Pos 13,C 50",key=x$: desc$ nokey NOKEYGLMSTR ioerr NOKEYGLMSTR
28620     close #glmstr: 
28640     L_ERR_OPEN_FOR_DESC: !
28660  ! reformat it from a read key= ready format to an input ready format
28680     if use_sub=0 then 
28700       let x$(10:12)="" 
28720     else 
28740       let x$(10:12)="-"&trim$(x$(10:12))
28760     end if
28780     let x$(4:9)=trim$(x$(4:9))
28800     if use_dept =0 then 
28820       let x$(1:3)="" 
28840     else 
28860       let x$(1:3)=trim$(x$(1:3))&"-"
28880     end if
32000 DONE: ! 
32020 ! pr ' fnRgl$ returned "'&(trim$(rpad$(x$,14)&desc$))(1:ReturnMaxLength)&'"'
32040     fnrgl$=(trim$(rpad$(x$,14)&desc$))(1:ReturnMaxLength)
32060     goto XIT
32080 ! _______________________________________________________________________
34000 NOKEYGLMSTR: ! 
34020     close #glmstr: 
34040     let x$="": desc$=""
34060     goto DONE
48000 XIT: fnend 
62000 ! <Updateable Region: ERTN>
62020 ERTN: fnerror(program$,err,line,act$,"xit")
62040     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
62060     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62080     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
62100 ERTN_EXEC_ACT: execute act$ : goto ERTN
62120 ! /region
