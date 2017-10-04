00010 ! Replace S:\acsGL\fnglfs
00020 ! ask questions for General Ledger Financial Statements
14000 def library fnglfs
14020   if glfsSetup<>val(env$('cno')) then ! r:
14040     glfsSetup=val(env$('cno'))
14060     library 'S:\Core\Library': fngethandle,fntos,fnlbl,fncomboa,fntxt,fnacs,fnerror,fncmdset,fncomboa,fnps,fnfscode,fnpriorcd,fnprocess,fnactpd,fnactpd$
14080     on error goto ERTN
14100     open #company=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
14120     read #company,using 'Form Pos 296,n 2,Pos 384,N 2',rec=1: lmu,nap
14140     ! lmu = Last Accounting Period Closed
14160     ! nap = Number of Accounting Periods
14180     close #company: 
14200     dim formatOption$(2)
14220     dim priorOrCurrentOption$(2)
14260     let formatOption$(1)="Primary" : let formatOption$(2)="Secondary" 
14280     let priorOrCurrentOption$(1)="Current" : let priorOrCurrentOption$(2)="Prior" 
14300     dim periodOption$(13)
14320     mat periodOption$(nap)
14340     for j=1 to nap : let periodOption$(j)=str$(j): next j 
14980   end if ! /r
16000   actpd$=fnactpd$ 
16020   actpd=fnactpd
16040   if fnprocess=1 then 
16060     fnps(1)
16080     fnpriorcd(1)
16100   else
22000     fntos(sn$="glFS-lib") 
22020     let lc=rc=0 : let mylen=23 : let mypos=mylen+3
22040     fnlbl(lc+=1,1,"Statement Format:",mylen,1)
22060     fncomboa("ps",lc,mypos,mat formatOption$) 
22080     let resp$(resp_format:=rc+=1)=formatOption$(1)
22100     fnlbl(lc+=1,1,"Year:",mylen,1)
22120     fncomboa("PriorCD",lc,mypos,mat priorOrCurrentOption$) 
22140     let resp$(resp_priorOrCurrent:=rc+=1)=priorOrCurrentOption$(1)
22160     fnlbl(lc+=1,1,"Period to Print:",mylen,1)
22180     fncomboa("FSCode",lc,mypos,mat periodOption$) 
22200     let resp$(resp_period:=rc+=1)=str$(actpd) ! periodOption$(1)
22220     fncmdset(3)
22240     fnacs(sn$,0,mat resp$,ckey)
32000     if ckey=5 then 
32020       fnglfs=5 
32040     else
32060       format=srch(mat formatOption$,resp$(resp_format))
32080       priorOrCurrent=srch(mat priorOrCurrentOption$,resp$(resp_priorOrCurrent))
32100       period=srch(mat periodOption$,resp$(resp_period))
32180       ! pr 'format=';format 
32200       ! pr 'priorOrCurrent=';priorOrCurrent
32220       ! pr 'period=';period 
32240       ! pause
33000       fnps(format)
33020       fnpriorcd(priorOrCurrent)
33040       fnfscode(period)
34420     end if
34440   end if
34460   XIT: ! 
34480 fnend 
87000 ! <Updateable Region: ERTN>
87020 ERTN: let fnerror(program$,err,line,act$,"xit")
87040     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
87060     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
87080     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
87100 ERTN_EXEC_ACT: execute act$ : goto ERTN
87120 ! /region