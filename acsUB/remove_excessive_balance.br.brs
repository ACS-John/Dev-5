00010   library 'S:\Core\Library': fnxit,fncno,fnopenprn,fncloseprn,fnerror,fnwait,fndate_mmddyy_to_ccyymmdd,fnpause,fnLastBillingDate,fngethandle,fntop,fnTos,fnLbl,fnTxt,fncmbact,fncmbrt2,fnChk,fnCmdSet,fnAcs,fnmsgbox ! fncombof
00030   on error goto ERTN
00040   fncno(cno) ! get account first
00050   fn_main_loop
01010   def fn_options(&route,&billingdate$) ! show options dialog to user and return selections
01020     dim screen_name$*100,resp$(4)*255
01030     fnTos(screen_name$="UndoBillingOptions")
01050 ! screen instructions
01060     fnLbl(2,2,"Use the options below to limit the customers to reverse.")
01070     fnLbl(3,2,"Warning: only the most recent billing date can be reversed for any account.")
01080 ! 
01140 ! combo for route selection
01150     fnLbl(7,2,"Select a route (or undo all):")
01160     fncmbrt2(7,35)
01170     resp$(1)="[All]"
01180 ! 
01190 ! billing date text box
01200     fnLbl(9,2,"Billing date:")
01210     fnTxt(9,35,8,0,0,"1")
01220     fnLastBillingDate(lastbilling) ! get last billing date and use it for the default
01230     resp$(2)=str$(lastbilling)
01270 ! 
01280     fnCmdSet(2) ! show "Next" and "Cancel" buttons
01290 ! 
01300     fnAcs(screen_name$,0,mat resp$,ckey) ! run the screen
01310 ! 
01320     if ckey=5 then ! if user pressed Cancel
01330       fn_options=0
01340     else 
01360       if resp$(1)="[All]" then route=0 else route=val(resp$(1))
01370       billingdate$=resp$(2)
01390 ! 
01400       fn_options=1
01410     end if 
01420   fnend  ! fn_Options
01430   def fn_openfiles
01440     open #(f_custacct:=fngethandle): "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno'),internal,outIn,keyed 
01450     open #(f_trans:=fngethandle): "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno'),internal,outIn,keyed 
01460   fnend 
01770   def fn_printheader
01780     pg+=1
01790     pr #255: cap$
01800     pr #255: "Page "&str$(pg)
01810     pr #255: ""
01820     pr #255: "All accounts listed have been modified."
01830     pr #255: ""
01840     pr #255: "Account           Billing Date"
01850     pr #255: "_______________   ____________"
01860   fnend 
01880 ! <Updateable Region: ERTN>
01890 ERTN: fnerror(program$,err,line,act$,"NO")
01900   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01910   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01920   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01930 ERTN_EXEC_ACT: execute act$ : goto ERTN
01940 ! </Updateable Region: ERTN>
10000   def fn_main_loop ! main
10020     dim cap$*80,billingdate$*10,msgtext$(1)*1000,readings(12),charges(12),breakdown(10),readingdates(2)
10040     fntop(program$,cap$="Remove Excessive Balance")
10060     remove_total=2
10080     do 
10100       cont=fn_options(route,billingdate$) ! collect user options
10120       if trim$(billingdate$)="0" then valid=0 else valid=1
10140       mat msgtext$(1:1)=("You must enter a billing date")
10160       if valid=0 then let fnmsgbox(mat msgtext$,answer$,"Invalid Entry",0)
10180     loop while not valid
10200 ! 
10220     mat msgtext$(5)
10240     msgtext$(1) = "Warning:"
10260     msgtext$(2) = "This action will reduce the balance and balance breakdown of all customers "
10280     if route <>0 then msgtext$(1)=msgtext$(1)&" (within route "&str$(route)&")"
10300     msgtext$(3) = "by "&str$(remove_total)&"x the amount of the billing on "&billingdate$&'.'
10320     msgtext$(4) = "This action is irreversible and should only be performed by an ACS Technician."
10340     msgtext$(5) = "Do you want to continue?"
10360     fnmsgbox(mat msgtext$,answer$,"Confirm Action",4)
10380     if (answer$<>"Yes") then cont=0
10400 ! 
10420     undocount=0
10440     if cont then 
10460       dim acct$*10,custname$*30,trcust$(3)*10,trdate(3)
10480 CUSTFORM: form c 10,x 30,c 30,pos 1741,n 2,pos 217,12*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1750,2*n 6
10520       fn_openfiles ! open data files
10530       open #h_iphold:=fngethandle: "Name="&env$('Q')&"\UBmstr\IpHold7.h"&env$('cno'),internal,input 
10540       fnopenprn : fn_printheader
10560       do 
10580 NEXT_CUSTOMER: ! 
10582         read #h_iphold,using 'form pos 1,C 10': z$ eof CUSTDONE
10600         read #f_custacct,using CUSTFORM,key=z$: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates eof CUSTDONE
10601 !       read #f_custacct,using CUSTFORM: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates eof CUSTDONE
10602 ! if trim$(acct$)='107000.00' then pause
10610         if chargedate=val(billingdate$) then 
10620           if route=0 or custroute=route then ! if a route was selected and customer doesn't match, skip customer
10640             if fn_get_trans then ! get latest and 2 prior charge transactions for this customer
10680               undocount+=1
10700               for remove_item=1 to remove_total
10720                 balance=balance-tamt
10740                 for item=1 to 9 ! assuming 10 is the penalty
10760                   breakdown(item)=breakdown(item)-tg(item)
10780                 next item
10800               next remove_item
10820               rewrite #f_custacct,using CUSTFORM: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates
10840               pr #255,using "form pos 5,c 10,x 5,pic(zz/zz/zz),X 5,N 10.2": acct$,str$(chargedate),balance pageoflow PRINTPAGEOVERFLOW
10860             else 
10880               pr #255: "Could not find transaction for account "&acct$
10900             end if 
10920           end if 
10930         end if 
10940       loop 
10960       goto CUSTDONE
10980 PRINTPAGEOVERFLOW: ! 
11000       pr #255: newpage
11020       fn_printheader
11040       continue 
11060 CUSTDONE: ! 
11120       mat msgtext$(1)=("Customers reversed: "&str$(undocount))
11140       fnmsgbox(mat msgtext$,answer$,"Report",0)
11160       fncloseprn
11180     end if 
11200 ! 
11220     fnxit ! exit program
11240   fnend  ! fn_main_loop
20000   def fn_get_trans
20020     dim transacct$*10
20040 ! 
20060     gt_return=0
20080     dateshouldbe=date(days(val(billingdate$),"mmddyy"),"ccyymmdd") : if str$(dateshouldbe)(1:2)="19" then dateshouldbe+=1000000
20100 ! 
20120     read #f_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(dateshouldbe)&"1": transacct$,transdate,transcode,tamt,mat tg,tnet,wread,wused,tbal,pcode nokey GT_FINIS
20140 TRANSFORM: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
20160     gt_return=1
20180 ! 
20200 GT_FINIS: ! 
20220     fn_get_trans=gt_return
20240   fnend 
