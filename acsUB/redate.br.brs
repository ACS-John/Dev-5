00010 ! Replace S:\acsUB\reDate
00011 ! Change Wrong Transaction Dates
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fntos,fnlbl,fnacs,fnwait,fntxt,fncmdset,fngethandle,fndate_mmddyy_to_ccyymmdd
00040   on error goto ERTN
00050 ! ______________________________________________________________________
10000   dim srv$(3)*1,cap$*128,txt$*80,tg(11)
10600   let fntop(program$,cap$="Change Wrong Transaction Dates")
10800 MAIN: ! 
11000   let sn$="redate"
11200   let fntos(sn$)
11600   let fnlbl(1,1,"Wrong Date:",22,1)
11800   let fntxt(1,23,8,0,0,"1")
12000   let resp$(1)='' ! '120101' ! '012011'
12400   let fnlbl(2,1,"Correct Date:",22,1)
12600   let fntxt(2,23,8,0,0,"1")
12800   let resp$(2)='' ! '120102' ! '012012'
13200   let fnlbl(4,1,"Lowest Record Number:",22,1)
13400   let fntxt(4,23,8,0,0,"0")
13600   let resp$(3)='0' ! str$(33430) ! str$(88000)
13800   let fncmdset(2)
14000   let fnacs(sn$,0,mat resp$,ckey)
14200   if ckey=5 then goto XIT
14400   let date_bad=fndate_mmddyy_to_ccyymmdd(val(resp$(1)))
14600   let date_good=fndate_mmddyy_to_ccyymmdd(val(resp$(2)))
14800   let rec_low=val(resp$(3))
15000   open #h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubTrIndx.h"&env$('cno')&",Shr",internal,outin,keyed 
15200   do 
15400     read #h_trans,using 'form pos 11,N 8': trans_date eof EO_TRANS
15500 ! if trans_date=date_bad then pause
15600     if (rec_low=0 or rec(h_trans)=>rec_low) and trans_date=date_bad then 
15810       let chg_count+=1
15820       rewrite #h_trans,using 'form pos 11,N 8': date_good
16000     end if  ! rec_low>0 and rec(h_trans)<=rec_low and trans_date=date_bad then
16200   loop 
16400 EO_TRANS: ! 
16600   close #h_trans: 
16800   goto XIT
17000 ! ______________________________________________________________________
17200 XIT: ! 
20000 ! pr 'chg_count=';chg_count : end
30000   let fnxit
80380 ! ______________________________________________________________________
80390 ! <Updateable Region: ERTN>
80400 ERTN: let fnerror(program$,err,line,act$,"xit")
80410   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80420   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80430   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
80440 ERTN_EXEC_ACT: execute act$ : goto ERTN
80450 ! /region
