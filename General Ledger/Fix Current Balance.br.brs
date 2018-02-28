00010 ! formerly S:\acsGL\CB
00020 ! fixes Current Balance, by taking Beginning Balance (or previous balance 2 yrs ago) and adding current transactions (optional a range of accumulated transactions too) to it.
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fngethandle,fnAcs,fnChk,fnLbl,fnTxt,fnTos,fnCmdSet,fnqgl,fnagl$,fnrgl$,fncreg_read,fncreg_write,fnGetFundList
00050   fntop(program$,"Fix Current Balance")
00060   on error goto ERTN
00100   dim bp(13)
00120   dim resp$(128)*256
00200   open #company=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input 
00260   read #company,using 'Form Pos 150,2*N 1,Pos 384,n 2': use_dept,use_sub,nap
00280   close #company: 
00320   fnGetFundList(mat fund_list)
28000   ! r: debug setup
28020              if env$('acsdeveloper')<>'' then 
28040                debug=1
28060                debug_gl$=' 51   830  0'
28080              end if
28100   ! /r
32000   if ~fn_theScreen then goto XIT
34000   ! r: setup and open files
34020   if enableProcessAccumulatedTrans$='True' then
34040     open #hAcTrans:=fngethandle: 'Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno]',internal,input,keyed
34100     startWithBalEndOfPriorYear=1
34120   end if
34140   open #hGlMstr:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,outIn,keyed 
34160   open #hGlTrans:=fngethandle: "Name=[Q]\GLmstr\GLTrans.H[cno],Shr",internal,input,relative 
34180   fTransBoth: form pos 1,c 12,n 6,pd 6.2
34200   ! /r
36000   do ! r: main loop

36020     read #hGlMstr,using 'Form POS 1,C 12,Pos 81,2*PD 6.2,pos 327,pd 6.2,pos 171,13*pd 6.2': gln$, bb,cb,pbp,mat bp eof XIT

36040     cbOrigional=cb
36060     activityCurrent=fn_currentActivity(hGlTrans,gln$)
36080     if enableProcessAccumulatedTrans$='True' then
36100       activityHistory=fn_accumulatedActivity(hAcTrans,gln$,dayStart,dayEnd)
36120     end if
36140     if startWithBalEndOfPriorYear then 
36160       isRetainedEarningsAccount=fn_is_a_retained_earn_account(gln$)
36180       if isRetainedEarningsAccount then
36200         cb=bp(nap)+activityCurrent+activityHistory 
36210         ! cb=pbp+activityCurrent+activityHistory
36220       else
36240         cb= 0 +activityCurrent+activityHistory
36260       end if
36280     else
36300       cb=bb+activityCurrent+activityHistory
36320     end if
38000     if debug and gln$=debug_gl$ then ! r: display debug information
38020       pr 'GL Number='&gln$
38040       pr '       current balance before recalcution: ';cbOrigional
38060       if startWithBalEndOfPriorYear then 
38080         if isRetainedEarningsAccount then 
38090         pr '       balance end of last period last year=';bp(nap)
38100 !       pr '       beginning balance two years ago=';pbp
38120         else
38140         pr '       not a retained earnings account start with 0'
38160         end if
38180       else
38200         pr '       beginning balance =';bb
38220       end if
38240       pr '       activity from Current Transactions=';activityCurrent
38260       if enableProcessAccumulatedTrans$='True' then
38280         pr '       activity from Accumulated Transactions=';activityHistory
38300       end if
38320       pr '       new current balance is=';cb
38340       pause
38360     end if ! /r
42000     rewrite #hGlMstr,using 'Form POS 81,2*PD 6.2': bb,cb
42020   loop ! /r
49000 XIT: fnxit
52000   def fn_is_a_retained_earn_account(gl$)
52020 ! pr 'gl number passed is *'&gl$&'*'
52040 ! pr 'gl number last retained earnings *'&last_retained_earnings_acct$&'*'
52060     gl$=trim$(fnagl$(gl$))
52080     if use_dept then 
52100       fund_compare=val(gl$(1:3))
52120       fund_which=srch(mat fund_list,fund_compare)
52140     else 
52160       fund_which=1
52180     end if 
52200     if gl$<=trim$(last_retained_earnings_acct$(fund_which)) then 
52220 !     pr '"'&gl$&'"<="'&trim$(last_retained_earnings_acct$(fund_which))&'" so it IS a retained earnings account - fund:'&str$(fund_which)
52240       iarea_return=1
52260 !     pause
52280     else 
52300 !     pr '"'&gl$&'">"'&trim$(last_retained_earnings_acct$(fund_which))&'" so it is NOT a retained earnings account - fund:'&str$(fund_which)
52320       iarea_return=0
52340 !     pause
52360     end if 
52380     fn_is_a_retained_earn_account=iarea_return
52400   fnend 
54000 def fn_theScreen ! lots of local variables
54020   fncreg_read(cap$&': enableProcessAccumulatedTrans',enableProcessAccumulatedTrans$,'False')
54040   fncreg_read(cap$&': dayStart',tmp$) : dayStart=val(tmp$)
54060   fncreg_read(cap$&': dayEnd',tmp$) : dayEnd=val(tmp$)
54080   fnTos(sn$=Cap$)
54100   rc=0
54120
54140   fnLbl(lc+=1,1,'WARNING: This program recalculates all the Current Balance files in General Ledger Accounts.')
54160   fnLbl(lc+=1,1,'Normally this program rebuilds the current balance from current transactions only.')
54180   fnLbl(lc+=1,1,'However you may choose to process History Transactions too.')
54200   fnLbl(lc+=1,1,'    If you do process History Transactions you must use a date range also')
54220   fnLbl(lc+=1,1,'    If you do process History Transactions the base for the date will be ')
54240   fnLbl(lc+=1,1,'          Previous Balance Two Years ago instead of Beginning Balance.')
54260   lc+=1 : mylen=14 : mypos=mylen+2 
54280   fnChk(lc+=1,1,'Process History Transactions')
54300   resp$(resp_enableAcTrans:=rc+=1)=enableProcessAccumulatedTrans$
54310 lc+=1
54320   fnLbl(lc+=1,1,'Starting Date:',mylen,1,0,0,0,"Enter a date to filter results or blank for all")
54340   fnTxt(lc,mypos,10,0,1,"3",0,"Enter a date to filter results or blank for all",0) 
54360   resp$(resp_dateStart=rc+=1)=date$(dayStart,'ccyymmdd')
54380   fnLbl(lc+=1,1,'Ending Date:',mylen,1,0,0,0,"Enter a date to filter results or blank for all")
54400   fnTxt(lc,mypos,10,0,1,"3",0,"Enter a date to filter results or blank for all",0) 
54420   resp$(resp_dateEnd=rc+=1)=date$(dayEnd,'ccyymmdd')
56000 ! 
56020     lc+=1 : col3_pos=mypos+20
56410     resp_lrea_fund_1=rc+1
56420     if use_dept then 
56440       col4_pos=col3_pos+10
56460       fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account(s)')
56500       for fund_item=1 to udim(mat fund_list)
56520         fnLbl(lc+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",9,1)
56540         fnqgl(lc,col4_pos)
56560         rc+=1
56580         fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
56600       next fund_item
56620     else 
56630       col4_pos=col3_pos+32
56640       fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account:',31,1)
56660       fnqgl(lc,col4_pos)
56680       rc+=1
56700       fncreg_read("last retained earnings account - no fund ",resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
56720     end if 
58000   fnCmdSet(2)
58020   fnAcs(sn$,0,mat resp$,ckey)
58040   if ckey=5 then 
58060     theScreenReturn=0
58080   else
58100     theScreenReturn=1
58120     dayStart=days(resp$(resp_dateStart),'ccyymmdd')
58140     dayEnd=days(resp$(resp_dateEnd),'ccyymmdd')
58160     enableProcessAccumulatedTrans$=resp$(resp_enableAcTrans)
58180     fncreg_write(cap$&': enableProcessAccumulatedTrans',enableProcessAccumulatedTrans$)
58200     fncreg_write(cap$&': dayStart',str$(dayStart))
58220     fncreg_write(cap$&': dayEnd',str$(dayEnd))
59000     rc=resp_lrea_fund_1-1
59020     if use_dept then 
59030       mat last_retained_earnings_acct$(udim(mat fund_list))
59040       for fund_item=1 to udim(mat fund_list)
59060         last_retained_earnings_acct$(fund_item)=fnagl$(resp$(rc+=1))
59080         fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
59100       next fund_item
59120     else 
59140       last_retained_earnings_acct$(1)=fnagl$(resp$(rc+=1))
59160       fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
59180     end if 
59200   end if  ! ck<>5
59220   fn_theScreen=theScreenReturn
59920 fnend
65000 ! <Updateable Region: ERTN>
65020 ERTN: fnerror(program$,err,line,act$,"xit")
65040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
65060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
65080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
65100 ERTN_EXEC_ACT: execute act$ : goto ERTN
65120 ! /region
70000 def fn_currentActivity(hGlTrans,gln$)
70020   caReturn=0
70040   restore #hGlTrans:
71000   do 
71020     read #hGlTrans,using fTransBoth: trgl$,tr_date,tr_amt eof caFinis
71080     if trgl$(1:3)  ="   " then trgl$(1:3)  ="  0"
71100     if trgl$(4:9)  ="   " then trgl$(4:9)  ="  0"
71120     if trgl$(10:12)="   " then trgl$(10:12)="  0"
73000     if trgl$=gln$ then 
73020         if debug and gln$=debug_gl$ then
73040           pr 'adding $'&str$(tr_amt)&' from '&date$(dayTran,'mm/dd/ccyy')&' from Current Transactions'
73060         end if
73080       caReturn+=tr_amt
73100     end if
73120   loop 
74000   caFinis: ! 
74020   fn_currentActivity=caReturn
74040 fnend 
80000 def fn_accumulatedActivity(hAcTrans,gln$*12,dayStart,dayEnd)
80020   aaReturn=0
80040   restore #hAcTrans,search=>gln$: nokey aaFinis
81000   do 
81020     read #hAcTrans,using fTransBoth: trgl$,tr_date,tr_amt eof aaFinis
81040     dayTran=days(tr_Date,'mmddyy')
81060     if (dayStart=0 or dayTran=>dayStart) and (dayEnd=0 or dayTran<=dayEnd) then
81080       if trgl$(1:3)  ="   " then trgl$(1:3)  ="  0"
81100       if trgl$(4:9)  ="   " then trgl$(4:9)  ="  0"
81120       if trgl$(10:12)="   " then trgl$(10:12)="  0"
83000       if trgl$=gln$ then 
83020         if debug and gln$=debug_gl$ then
83040           pr 'adding $'&str$(tr_amt)&' from '&date$(dayTran,'mm/dd/ccyy')&' from Accumulated Transactions'
83060         end if
83080         aaReturn+=tr_amt
83100       end if
83120     end if
83140   loop while gln$=trgl$
84000   aaFinis: ! 
84020   fn_accumulatedActivity=aaReturn
84040 fnend 

