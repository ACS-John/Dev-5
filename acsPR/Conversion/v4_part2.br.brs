10000 ! Replace S:\acsPR\conversion\v4_part2
10020   fn_setup
10180 ! ______________________________________________________________________
10200   fntop("S:\acsPR\conversion\v4_part2",cap$="add missing files and indexes")
10220 ! r: do every company - loop top
10240   fngetdir2(env$('Q')&'\'&fncursys$&"mstr",mat filename$,'/od /ta',"Company.*")
10260   company_count=filename_item=0
10280   for filename_item=1 to udim(mat filename$)
10300     let tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
10320     if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
10340       cno=tmp_cno
10360 ! 
10380 ! let fncno(cno)
10400 ! 
10420 ! /r
10440       fn_pr_conversion_add_missing(cno)
10460 ! r: do every company - loop bottom
10480     end if 
10500 ACNO_CONV: ! 
10520   next filename_item
10540 ! /r
10560   end  ! 
20000   def fn_setup
20020     library 'S:\Core\Library': fntop,fnwait,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fntop,fngetdir2,fnputcno,fnstatus,fncursys$,fnindex_it,fnindex_sys
20040     on error goto ERTN
20060 ! ______________________________________________________________________
20080     dim a$*40,em$*30,ta(2),cp(32),tcp(22),hc(5),thc(5),d$*20,whc(10)
20100     dim dedcode(10),calcode(10),dedfed(10),message$*40,cap$*128
20120     dim tcp(32),newtdc(10),newtdet(23),tdt(4),tcd(3),tdet(17),tdy(6),tdc(6)
20140     dim ty(21),tqm(17),tcp(22),tdet(17),dednames$(20)*20,d1$*20
20160   fnend 
30000   def library fnpr_conversion_add_missing(cno)
30020     if ~setup then let fn_setup
30040     fnpr_conversion_add_missing=fn_pr_conversion_add_missing(cno)
30060   fnend 
40000   def fn_pr_conversion_add_missing(cno)
40020 !   pr 'all files should be closed now' : pause
40080     fnstatus('PR adding missing files and indexes - Company Number '&str$(cno))
40160     dim company_file$(1)*128,tmp$*256
40180     let tmp$=env$('Q')&"\PRmstr"
40320     open #14: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&str$(cno),internal,outin,keyed ioerr L2180
40340     goto L2200
40360 L2180: ! 
40380     open #14: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&str$(cno)&",RecL=224,kps=1,kln=17,replace",internal,outin,keyed 
40400 L2200: ! 
40420     close #14: 
40440 !   fnindex_it(env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno),env$('Q')&"\PRmstr\checkidx3.h"&str$(cno),"1/12/9 8/6/3")
40460 ! 
40480     open #breakdown=31: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno)&",RecL=39,KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno)&",kps=1/9/14,kln=8/5/8,replace",internal,outin,keyed 
40500     close #breakdown: 
40520 !   fnindex_it(env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno),env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno),"1/9/14 8/5/8")
40540 ! 
40560     open #30: "Name="&env$('Q')&"\PRmstr\dd.h"&str$(cno)&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&str$(cno)&",kps=1,kln=10,Use",internal,outin,keyed 
40580     close #30: 
40600 !   fnindex_it(env$('Q')&"\PRmstr\dd.H"&str$(cno),env$('Q')&"\PRmstr\ddidx1.H"&str$(cno),"1,10")
40620 ! 
40680     fnindex_sys(cno,'PR')
40700     goto XIT
40720 XIT: ! 
40740   fnend 
50000 ! <Updateable Region: ERTN>
50020 ERTN: let fnerror(program$,err,line,act$,"xit")
50040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
50060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50100 ERTN_EXEC_ACT: execute act$ : goto ERTN
50120 ! /region
