10000 ! S:\acsGL\adjustaccounts
10010 ! ______________________________________________________________________
10020   library 'S:\Core\Library': fntop,fnxit,fncno,fnerror,fncursys$,fntos,fnlbl,fntxt,fncmdset,fnacs,fnagl$,fnqglbig,fnrglbig$,fngethandle,fnmsgbox,fncmdkey
10030   on error goto ERTN
10040 ! ______________________________________________________________________
10050   dim cap$*128,resp$(3)*255,ymbal(13),priorym(13)
10060   fntop(program$,cap$="Adjust Account Balances")
10070   fn_adjustaccounts
10080   def fn_adjustaccounts
10090     fncno(cno)
10100     open #(h_glmstr:=fngethandle): "Name="&env$('Q')&"\GLmstr\glmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLindex.h"&env$('cno')&",Shr",internal,outin,keyed 
10110 GLMSTR: form n 3,n 6,n 3,pos 81,28*pd 6.2
10120     open #(h_actrans:=fngethandle): "Name="&env$('Q')&"\GLmstr\actrans.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\ACTRIDX.h"&env$('cno')&",Shr",internal,input,keyed 
10130 ACTRANS: form n 3,n 6,n 3,n 6,pd 6.2,pos 71,n 2
10140     open #(h_gltrans:=fngethandle): "Name="&env$('Q')&"\GLmstr\gltrans.h"&env$('cno')&",Shr",internal,input 
10150 GLTRANS: form n 3,n 6,n 3,n 6,pd 6.2
10160     do 
10170       fn_getadjustment
10180       if gl$<>'' then 
10190         read #h_glmstr,using GLMSTR,key=gl$: dept,major,sub,startbal,balance,mat ymbal,mat priorym
10200         balance=adjustamt
10210         read #h_actrans,using ACTRANS,key>=rpad$(gl$,20): trdept,trmajor,trsub,trdate,tramt,pcode eof ACTRANSDONE
10220         do 
10230           if trdept<>dept or trmajor<>major or trsub<>sub then goto ACTRANSDONE
10240           if date(days(trdate,'mmddyy'),'ccyymmdd')>date(days(adjustdate,'mmddyy'),'ccyymmdd') then 
10250             if oldpcode>pcode then ! this means we're going back to the first period, so move our period values back into the previous yrmo and clear out current
10260               for period=1 to 13
10270                 priorym(period)=ymbal(period) : ymbal(period)=0
10280               next period
10290               startbal=priorym(oldpcode)
10300             end if 
10310             oldpcode=pcode
10320             balance+=tramt
10330             ymbal(pcode)=balance
10340           end if 
10350           read #h_actrans,using ACTRANS,next: trdept,trmajor,trsub,trdate,tramt,pcode eof ACTRANSDONE
10360         loop 
10370 ACTRANSDONE: restore #h_gltrans: 
10380         do 
10390           read #h_gltrans,using GLTRANS: trdept,trmajor,trsub,trdate,tramt eof GLTRANSDONE
10400           if date(days(trdate,'mmddyy'),'ccyymmdd')>date(days(adjustdate,'mmddyy'),'ccyymmdd') and trdept=dept and trmajor=major and trsub=sub then 
10410             balance+=tramt
10420           end if 
10430         loop 
10440 GLTRANSDONE: if fn_confirmadjustment then rewrite #h_glmstr,using GLMSTR: dept,major,sub,startbal,balance,mat ymbal,mat priorym
10450       end if 
10460     loop while gl$<>''
10470     close #h_glmstr: 
10480     close #h_actrans: 
10490     close #h_gltrans: 
10500     fnxit
10510   fnend 
10520   def fn_getadjustment
10530     mat resp$(3)
10540     fntos("accountadjust")
10550     mylen=23: mypos=mylen+3 : right=1
10560     fnlbl(1,1,"General Ledger Number:",mylen,right)
10570     fnqglbig(1,mypos,0,2) : resp$(1)=fnrglbig$(gl$)
10580     fnlbl(2,1,"Date of Balance:",mylen,right)
10590     fntxt(2,mypos,8,8,1,"1",0) : resp$(2)=cnvrt$("pic(zzzzzz)",adjustdate)
10600     fnlbl(3,1,"Balance on Date:",mylen,right)
10610     fntxt(3,mypos,14,0,right,"10",0) : resp$(3)=''
10620     fncmdset(11)
10630     fnacs("accountadjust",0,mat resp$,ckey)
10640     if ckey=1 then 
10650       gl$=fnagl$(resp$(1)) : adjustdate=val(resp$(2)) : adjustamt=val(resp$(3))
10660     else 
10670       gl$=''
10680     end if 
10690   fnend 
10700   def fn_confirmadjustment
10710     mat resp$(27)
10720     fntos("adjustconfirm")
10730     mylen=23 : mypos=mylen+3 : right=1
10740     fnlbl(1,1,"Account "&gl$,mylen)
10750     fnlbl(2,1,"Current Balance:",mylen,right)
10760     fntxt(2,mypos,14,0,right,"10",0) : resp$(1)=str$(balance)
10770     for period=1 to 13
10780       fnlbl(3+period,1,"Period "&str$(period)&":",mylen,right)
10790       fntxt(3+period,mypos,14,0,right,"10",0) : resp$(period+1)=str$(ymbal(period))
10800     next period
10810     for period=1 to 13
10820       fnlbl(3+period,mypos+20,"Prior:",mylen,right)
10830       fntxt(3+period,mypos*2+20,14,0,right,"10",0) : resp$(period+14)=str$(priorym(period))
10840     next period
10850     fncmdkey("&Save",1,1)
10860     fncmdkey("Clear &Periods",2,0)
10870     fncmdkey("&Cancel",5,0,1)
10880     fnacs("adjustconfirm",0,mat resp$,ckey)
10890     if ckey=1 then 
10900       balance=val(resp$(1))
10910       for period=1 to 13 : ymbal(period)=val(resp$(period+1)) : priorym(period)=val(resp$(period+14)) : next period
10920       fn_confirmadjustment=1
10930     else if ckey=2 then 
10940       for period=1 to 13
10950         priorym(period)=ymbal(period) : ymbal(period)=0
10960       next period
10970       startbal=priorym(oldpcode)
10980       fn_confirmadjustment=fn_confirmadjustment
10990     else 
11000       dim mg$(2)*255
11010       mg$(1) = "Warning: your balance adjustments for account "&gl$&" were NOT saved!"
11020       mg$(2) = "You can retry on the next screen, or modify another account."
11030       fnmsgbox(mat mg$,resp$,"Balance Adjustment Canceled",0)
11040       fn_confirmadjustment=0
11050     end if 
11060   fnend 
11070 ! <Updateable Region: ERTN>
11080 ERTN: fnerror(program$,err,line,act$,"xit")
11090   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
11100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
11110   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
11120 ERTN_EXEC_ACT: execute act$ : goto ERTN
11130 ! /region
11140 ! ______________________________________________________________________
