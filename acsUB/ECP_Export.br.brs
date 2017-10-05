10000 ! similar to S:\Utility Billing\Create Hand Held File
10020 ! -- Tranfer Data From Computer to Hand Held
10040 ! r: setup
10060   library 'S:\Core\Library': fnerror,fntos,fnlbl,fnacs,fnxit,fncno,fncmdset,fntop,fnmsgbox,fntxt,fngethandle,fnclient_has,fnureg_read,fnureg_write,fnget_services
10080   on error goto ERTN
11000 ! ______________________________________________________________________
11020   dim gb(10),ab$(3)*30
11120   dim z$*10
11140   dim rw4(22,13)
11160   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
11200   dim delim$*1
11220   dim resp$(3)*256
11240   dim cap$*128
11280   dim servicename$(10)*20
11300   dim extra(23),extra$(11)*30
11320   dim exp_filename$*256
11340 ! ______________________________________________________________________
12000   fncno(cno)
12020   fntop(program$,cap$="Export for External Collections Process")
12040 ! 
14000   if ~fnclient_has('U5') then 
14020     mat m$(2)
14040     m$(1)="You must purchase the ACS Utility Billing External Collections Processing"
14060     m$(2)="module to access these features"
14080     fnmsgbox(mat m$, response$, cap$,64)
14100     goto XIT
14120   end if 
14140 ! 
14160   fnget_services(mat servicename$) : for servicename_item=1 to udim(mat servicename$) : servicename$(servicename_item)=trim$(servicename$(servicename_item)) : next servicename_item
14180   delim$=chr$(9)
14200 ! 
16000   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed 
16020   open #h_alt_bill:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
16040 ! 
16060   fnureg_read('ECP Export Filename',exp_filename$)
16080   if exp_filename$='' then exp_filename$=os_filename$(env$('userprofile')&'\Desktop')&"\ACS_ECP_Export.txt"
16100 ! /r
20000 MENU1: ! 
20020   fntos(sn$="ecp_export")
20040   fnlbl(1,1,"Destination Path and File Name:",34,1)
20060   fntxt(1,36,40,256,0,"71")
20080   resp$(1)=exp_filename$
20100   fnlbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
20120   fncmdset(2)
20140   fnacs(sn$,0,mat resp$,ckey)
20160   if ckey=5 then goto XIT
20180   exp_filename$=resp$(1)
20200 ! 
20220   open #h_ecp:=fngethandle: "Name="&env$('at')&br_filename$(exp_filename$)&",Size=0,RecL=2500,Replace,EOL=CRLF",display,output ioerr MENU1
20240   exp_filename$=os_filename$(file$(h_ecp))
20260   fnureg_write('ECP Export Filename',exp_filename$)
20280 ! restore #h_customer:
20370 ! r: main loop
20940   gosub HEADER ! work in progress
20960   do 
20980     read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,mat extra$ eof FINIS
21000 F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,pos 1864,c 30,7*c 12,3*c 30
21020     gosub ALT_BILL_ADR
21040     pr #h_ecp: z$&delim$; !    1
21060     pr #h_ecp: e$(1)&delim$; !    1.5         Meter Address
21080     pr #h_ecp: e$(2)&delim$; !    2         Name
21100     pr #h_ecp: e$(3)&delim$; !    3          addr 1
21120     pr #h_ecp: extra$(1)&delim$; !    4          addr 2
21140     pr #h_ecp: e$(4)&delim$; !    5
21160     pr #h_ecp: f$(1)&delim$; !    6
21180     pr #h_ecp: str$(a(1))&delim$; !   7
21200     pr #h_ecp: str$(a(2))&delim$; !   8
21220     pr #h_ecp: str$(a(3))&delim$; !   9
21240     pr #h_ecp: str$(a(4))&delim$; !   10
21260     pr #h_ecp: str$(a(5))&delim$; !   11
21280     pr #h_ecp: str$(a(6))&delim$; !   12
21300     pr #h_ecp: str$(a(7))&delim$; !   13
21320     pr #h_ecp: str$(b(1))&delim$; !   14
21340     pr #h_ecp: str$(b(2))&delim$; !   15
21360     pr #h_ecp: str$(b(3))&delim$; !   16
21380     pr #h_ecp: str$(b(4))&delim$; !   17
21400     pr #h_ecp: str$(b(5))&delim$; !   18
21420     pr #h_ecp: str$(b(6))&delim$; !   19
21440     pr #h_ecp: str$(b(7))&delim$; !   20
21460     pr #h_ecp: str$(b(8))&delim$; !   21
21480     pr #h_ecp: str$(b(9))&delim$; !   22
21500     pr #h_ecp: str$(b(10))&delim$; !   23
21520     pr #h_ecp: str$(b(11))&delim$; !   24
21540     pr #h_ecp: str$(c(1))&delim$; !   25
21560     pr #h_ecp: str$(c(2))&delim$; !   26
21580     pr #h_ecp: str$(c(3))&delim$; !   27
21600     pr #h_ecp: str$(c(4))&delim$; !   28
21620     pr #h_ecp: str$(d(1))&delim$; !   29
21640     pr #h_ecp: str$(d(2))&delim$; !   30
21660     pr #h_ecp: str$(d(3))&delim$; !   31
21680     pr #h_ecp: str$(d(4))&delim$; !   32
21700     pr #h_ecp: str$(d(5))&delim$; !   33
21720     pr #h_ecp: str$(d(6))&delim$; !   34
21740     pr #h_ecp: str$(d(7))&delim$; !   35
21760     pr #h_ecp: str$(d(8))&delim$; !   36
21780     pr #h_ecp: str$(d(9))&delim$; !   37
21800     pr #h_ecp: str$(d(10))&delim$; !    38
21820     pr #h_ecp: str$(d(11))&delim$; !   39
21840     pr #h_ecp: str$(d(12))&delim$; !   40
21860     pr #h_ecp: str$(d(13))&delim$; !   41
21880     pr #h_ecp: str$(d(14))&delim$; !   42
21900     pr #h_ecp: str$(d(15))&delim$; !   43
21920     pr #h_ecp: str$(bal)&delim$; !    44
21940     pr #h_ecp: str$(f)&delim$; !    45
21960     pr #h_ecp: str$(g(1))&delim$; !   46
21980     pr #h_ecp: str$(g(2))&delim$; !   47
22000     pr #h_ecp: str$(g(3))&delim$; !   48
22020     pr #h_ecp: str$(g(4))&delim$; !   49
22040     pr #h_ecp: str$(g(5))&delim$; !   50
22060     pr #h_ecp: str$(g(6))&delim$; !   51
22080     pr #h_ecp: str$(g(7))&delim$; !   52
22100     pr #h_ecp: str$(g(8))&delim$; !   53
22120     pr #h_ecp: str$(g(9))&delim$; !   54
22140     pr #h_ecp: str$(g(10))&delim$; !   55
22160     pr #h_ecp: str$(g(11))&delim$; !   56
22180     pr #h_ecp: str$(g(12))&delim$; !   57
22200     pr #h_ecp: alp$&delim$; !    58
22220     pr #h_ecp: f$(2)&delim$; !    59
22240     pr #h_ecp: f$(3)&delim$; !    60
22260     pr #h_ecp: str$(bra)&delim$; !    61
22280     pr #h_ecp: str$(gb(1))&delim$; !   62
22300     pr #h_ecp: str$(gb(2))&delim$; !   63
22320     pr #h_ecp: str$(gb(3))&delim$; !   64
22340     pr #h_ecp: str$(gb(4))&delim$; !   65
22360     pr #h_ecp: str$(gb(5))&delim$; !   66
22380     pr #h_ecp: str$(gb(6))&delim$; !   67
22400     pr #h_ecp: str$(gb(7))&delim$; !   68
22420     pr #h_ecp: str$(gb(8))&delim$; !   69
22440     pr #h_ecp: str$(gb(9))&delim$; !   70
22460     pr #h_ecp: str$(gb(10))&delim$; !   71
22480     pr #h_ecp: ab$(1)&delim$; !    72
22500     pr #h_ecp: ab$(2)&delim$; !    73
22520     pr #h_ecp: ab$(3) !   74
22540   loop  ! /r
26000 ALT_BILL_ADR: ! r:
26020   mat ab$=("")
26040   read #h_alt_bill,using 'Form POS 11,3*C 30',key=z$: mat ab$ nokey ignore
26060   return  ! /r
32000 HEADER: ! r:
32020   pr #h_ecp: 'Account Key'&delim$;
32030   pr #h_ecp: 'Meter Address'&delim$;
32040   pr #h_ecp: 'Name'&delim$;
32060   pr #h_ecp: 'Address 1 - Primary'&delim$;
32080   pr #h_ecp: 'Address 2 - Primary'&delim$;
32100   pr #h_ecp: 'CSZ - Primary'&delim$;
32120   pr #h_ecp: servicename$(1)&' Meter Number'&delim$; ! f$(1)&delim$;
32140   pr #h_ecp: servicename$(1)&' Rate Code'&delim$; ! str$(a(1))&delim$;
32160   pr #h_ecp: servicename$(2)&' Rate Code'&delim$; ! str$(a(2))&delim$;
32180   pr #h_ecp: servicename$(3)&' Rate Code'&delim$; ! str$(a(3))&delim$;
32200   pr #h_ecp: servicename$(4)&' Rate Code'&delim$; ! str$(a(4))&delim$;
32220   pr #h_ecp: servicename$(5)&' Rate Code'&delim$; ! str$(a(5))&delim$;
32240   pr #h_ecp: servicename$(9)&' Rate Code'&delim$; ! str$(a(6))&delim$;
32260   pr #h_ecp: servicename$(10)&' Rate Code'&delim$; ! str$(a(7))&delim$;
32280   pr #h_ecp: ''&delim$; ! str$(b(1))&delim$;
32300   pr #h_ecp: ''&delim$; ! str$(b(2))&delim$;
32320   pr #h_ecp: ''&delim$; ! str$(b(3))&delim$;
32340   pr #h_ecp: servicename$(4)&' Standard Charge'&delim$; ! str$(b(4))&delim$;
32360   pr #h_ecp: ''&delim$; ! str$(b(5))&delim$;
32380   pr #h_ecp: ''&delim$; ! str$(b(6))&delim$;
32400   pr #h_ecp: ''&delim$; ! str$(b(7))&delim$;
32420   pr #h_ecp: servicename$(1)&' Deposit'&delim$; ! str$(b(8))&delim$;
32440   pr #h_ecp: servicename$(2)&' Deposit'&delim$; ! str$(b(9))&delim$;
32460   pr #h_ecp: servicename$(3)&' Deposit'&delim$; ! str$(b(10))&delim$;
32480   pr #h_ecp: servicename$(4)&' Deposit'&delim$; ! str$(b(11))&delim$;
32500   pr #h_ecp: servicename$(1)&' Deposit Date'&delim$; ! str$(c(1))&delim$;
32520   pr #h_ecp: servicename$(2)&' Deposit Date'&delim$; ! str$(c(2))&delim$;
32540   pr #h_ecp: servicename$(3)&' Deposit Date'&delim$; ! str$(c(3))&delim$;
32560   pr #h_ecp: servicename$(4)&' Deposit Date'&delim$; ! str$(c(4))&delim$;
32580   pr #h_ecp: servicename$(1)&' Reading - Current'&delim$; ! str$(d(1))&delim$;
32600   pr #h_ecp: servicename$(1)&' Reading - Prior'&delim$; ! str$(d(2))&delim$;
32620   pr #h_ecp: servicename$(1)&' Used - Current'&delim$; ! str$(d(3))&delim$;
32640   pr #h_ecp: servicename$(1)&' Used - YTD'&delim$; ! str$(d(4))&delim$;
32660   pr #h_ecp: servicename$(3)&' Reading - Current'&delim$; ! str$(d(5))&delim$;
32680   pr #h_ecp: servicename$(3)&' Reading - Prior'&delim$; ! str$(d(6))&delim$;
32700   pr #h_ecp: servicename$(3)&' KWH Used - Current'&delim$; ! str$(d(7))&delim$;
32720   pr #h_ecp: servicename$(3)&' KWH Used -YTD'&delim$; ! str$(d(8))&delim$;
32740   pr #h_ecp: servicename$(4)&' Reading - Current'&delim$; ! str$(d(9))&delim$;
32760   pr #h_ecp: servicename$(4)&' Reading - Prior'&delim$; ! str$(d(10))&delim$;
32780   pr #h_ecp: servicename$(4)&' Used-Current'&delim$; ! str$(d(11))&delim$;
32800   pr #h_ecp: servicename$(4)&' Used-YTD'&delim$; ! str$(d(12))&delim$;
32820   pr #h_ecp: 'Units Per Meter'&delim$; ! str$(d(13))&delim$;
32840   pr #h_ecp: 'Demand Multiplier'&delim$; ! str$(d(14))&delim$;
32860   pr #h_ecp: 'Demand Reading'&delim$; ! str$(d(15))&delim$;
32880   pr #h_ecp: 'Current Balance'&delim$; ! str$(bal)&delim$;
32900   pr #h_ecp: 'Date of Charge'&delim$; ! str$(f)&delim$;
32920   pr #h_ecp: servicename$(1)&' Charge'&delim$; ! str$(g(1))&delim$;
32940   pr #h_ecp: servicename$(2)&' Charge'&delim$; ! str$(g(2))&delim$;
32960   pr #h_ecp: servicename$(3)&' Charge'&delim$; ! str$(g(3))&delim$;
32980   pr #h_ecp: servicename$(4)&' Charge'&delim$; ! str$(g(4))&delim$;
33000   pr #h_ecp: servicename$(5)&' Charge'&delim$; ! str$(g(5))&delim$;
33020   pr #h_ecp: servicename$(6)&' Charge'&delim$; ! str$(g(6))&delim$;
33040   pr #h_ecp: servicename$(7)&' Charge'&delim$; ! str$(g(7))&delim$;
33060   pr #h_ecp: servicename$(8)&' Charge'&delim$; ! str$(g(8))&delim$;
33080   pr #h_ecp: servicename$(9)&' Charge'&delim$; ! str$(g(9))&delim$;
33100   pr #h_ecp: 'Net Bill'&delim$; ! str$(g(10))&delim$;
33120   pr #h_ecp: 'Gross Bill'&delim$; ! str$(g(11))&delim$;
33140   pr #h_ecp: ''&delim$; ! str$(g(12))&delim$;
33160   pr #h_ecp: 'Alpha Sort Field'&delim$; ! alp$&delim$;
33180   pr #h_ecp: servicename$(3)&' Meter Number'&delim$; ! f$(2)&delim$;
33200   pr #h_ecp: servicename$(4)&' Meter Number'&delim$; ! f$(3)&delim$;
33220   pr #h_ecp: 'Alternate Billing Address'&delim$; ! str$(bra)&delim$;
33240   pr #h_ecp: servicename$(1)&' Breakdown'&delim$; ! str$(gb(1))&delim$;
33260   pr #h_ecp: servicename$(2)&' Breakdown'&delim$; ! str$(gb(2))&delim$;
33280   pr #h_ecp: servicename$(3)&' Breakdown'&delim$; ! str$(gb(3))&delim$;
33300   pr #h_ecp: servicename$(4)&' Breakdown'&delim$; ! str$(gb(4))&delim$;
33320   pr #h_ecp: servicename$(5)&' Breakdown'&delim$; ! str$(gb(5))&delim$;
33340   pr #h_ecp: servicename$(6)&' Breakdown'&delim$; ! str$(gb(6))&delim$;
33360   pr #h_ecp: servicename$(7)&' Breakdown'&delim$; ! str$(gb(7))&delim$;
33380   pr #h_ecp: servicename$(8)&' Breakdown'&delim$; ! str$(gb(8))&delim$;
33400   pr #h_ecp: servicename$(9)&' Breakdown'&delim$; ! str$(gb(9))&delim$;
33420   pr #h_ecp: servicename$(10)&' Breakdown'&delim$; ! str$(gb(10))&delim$;
33440   pr #h_ecp: 'Name - Alternate Billing'&delim$; ! ab$(1)&delim$;
33460   pr #h_ecp: 'Address - Alternate Billing'&delim$; ! ab$(2)&delim$;
33480   pr #h_ecp: 'CSZ - Alternate Billing'&delim$ ! ab$(3)
33500   return  ! /r
50000 ! <Updateable Region: ERTN>
50020 ERTN: fnerror(program$,err,line,act$,"xit")
50040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
50060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50100 ERTN_EXEC_ACT: execute act$ : goto ERTN
50120 ! /region
56120 ! 
58000 FINIS: ! r: Transfer to or from Hand Held Computer
58020   close #h_customer: ioerr ignore
58040   close #h_alt_bill: ioerr ignore
58060   close #h_ecp: ioerr ignore
58080   fn_report_created_file(exp_filename$)
58100   goto XIT ! /r
60000 XIT: fnxit
60010 ! IGNORE: continue
65000   def fn_report_created_file(exp_filename_report$*512)
65090     dim m$(2)*512
65100     if exp_filename_report$<>'' and exp_filename_report$<>':CON:' then 
65120       mat m$(2)
65140       m$(1)="External Collections File created:"
65160       m$(2)=os_filename$(exp_filename_report$)
65180       fnmsgbox(mat m$, response$, cap$,64)
65200     end if 
65240   fnend 
