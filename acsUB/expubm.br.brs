20000 ! Replace S:\acsUB\expubm
20020 ! -- Export UB Master File
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fntop,fnxit,fnTos,fnLbl,fnTxt,fnAcs,fnxit,fnerror,fnCmdSet,fntop,fnreg_read,fnreg_write,fncustomer_address,fnget_services
20070   library 'S:\Core\Library': fngethandle
20080 ! ______________________________________________________________________
20100   on error goto ERTN
20120 ! ______________________________________________________________________
20140   dim gb(10),ab$(3)*30,flob$(5),scrb$(3)*24,inb$(3)
20160   dim ri1$(20),rm$*60,rm$(20)*60,ra1(20),ra(2)
20180   dim wf$(13),ws$(13)*30,iow$(16),i$(16)*70
20200   dim fld$*60,nam$*30,n$*30,fm$(22)*80,ul$*186,hd$*186
20220   dim hln$*78,hk$*9,inh$(20),lnh$(20)*78,hhdr$*60
20240   dim x$*10,scrid$(2)*75,p$*10,o(2),txt$(3)*80
20260   dim tr(4),lnadr(20),tc$(4)*12,si$*25
20280   dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
20300   dim dest$*256,csz$*40,first_name$*30,last_name$*30,cap$*128
20320   dim delim$*1,streetnam$*30,streetnum$*30,state$*30,city$*30,zip$*30
20340   dim resp$(3)*256,extra$(11)*30
20380   fntop(program$,cap$="Export UB Master File")
24000   dim serviceName$(10)*20
24020   fnget_services(mat serviceName$) : for sNitem=1 to udim(mat serviceName$) : serviceName$(sNitem)=trim$(serviceName$(sNitem)) : nex sNitem
34000 MENU1: ! r:
34020   fnTos(sn$="expubm")
34040   fnLbl(1,1,"Destination Path and File Name:",34,1)
34060   fnTxt(1,36,40,256,0,"71")
34080   fnreg_read('exp_ubm.path',resp$(1)) : if resp$(1)='' then resp$(1)=os_filename$(env$('userprofile')&'\Desktop')&"\ubm.txt"
34100   fnLbl(2,1,"Delimiter (ASCII Code):" ,34,1)
34120   fnTxt(2,36,3,0,0,"30")
34140   resp$(2)="9"
34160   fnLbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
34180   fnCmdSet(2)
34200   fnAcs(sn$,0,mat resp$,ckey)
38000   if ckey=5 then goto XIT
38020   dest$=resp$(1)
38040   delas=val(resp$(2))
38060   fnreg_write('exp_ubm.path',dest$)
38080   goto OPENS
38100 ! /r
42000 OP2ERR: ! r:
42020   goto MENU1
42040 ! /r
46000 OPENS: ! 
46020   delim$=chr$(delas)
46030   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&',shr',internal,outIn,relative 
46040   ! open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno'),internal,input,relative 
46060   open #2: "Name="&br_filename$(dest$)&",Size=0,RecL=2500,Replace,EOL=CRLF",display,output ioerr OP2ERR
46080 ! form pos 1,c 14,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1
46100 ! form pos 287,c 10,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1
46120   open #3: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
46140   gosub HEADER ! work in progress
48000   do 
48020     read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,finalBillingCode,mat extra$ eof DONE
48040     F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1821,n 1,pos 1864,c 30,c 12,c 12,c 12,c 12,c 12,c 12,c 12,c 30,c 30,c 30
48060     gosub ALT_BILL_ADR
48080     dim addr$(4)*40
48100     fncustomer_address(z$,mat addr$)
48120     pr #2: '"'&z$&'"'&delim$;
48130     pr #2: e$(1)&delim$&e$(2)&delim$&e$(3)&delim$&extra$(1)&delim$&e$(4)&delim$;
48132     pr #2: f$(1)&delim$&str$(a(1))&delim$&str$(a(2))&delim$&str$(a(3))&delim$&str$(a(4))&delim$&str$(a(5))&delim$&str$(a(6))&delim$&str$(a(7))&delim$;
48134     pr #2: str$(b(1))&delim$&str$(b(2))&delim$&str$(b(3))&delim$;
48140     pr #2: str$(b(4))&delim$&str$(b(5))&delim$&str$(b(6))&delim$&str$(b(7))&delim$&str$(b(8))&delim$&str$(b(9))&delim$&str$(b(10))&delim$&str$(b(11))&delim$;
48160     pr #2: str$(c(1))&delim$&str$(c(2))&delim$&str$(c(3))&delim$&str$(c(4))&delim$&str$(d(1))&delim$&str$(d(2))&delim$&str$(d(3))&delim$&str$(d(4))&delim$&str$(d(5))&delim$&str$(d(6))&delim$&str$(d(7))&delim$&str$(d(8))&delim$&str$(d(9))&delim$;
48180     pr #2: str$(d(10))&delim$&str$(d(11))&delim$&str$(d(12))&delim$&str$(d(13))&delim$&str$(d(14))&delim$&str$(d(15))&delim$&str$(bal)&delim$&str$(f)&delim$&str$(g(1))&delim$&str$(g(2))&delim$&str$(g(3))&delim$;
48200     pr #2: str$(g(4))&delim$&str$(g(5))&delim$&str$(g(6))&delim$&str$(g(7))&delim$&str$(g(8))&delim$&str$(g(9))&delim$&str$(g(10))&delim$&str$(g(11))&delim$&str$(g(12))&delim$&alp$&delim$&f$(2)&delim$&f$(3)&delim$;
48220     pr #2: str$(bra)&delim$&str$(gb(1))&delim$&str$(gb(2))&delim$&str$(gb(3))&delim$&str$(gb(4))&delim$&str$(gb(5))&delim$&str$(gb(6))&delim$&str$(gb(7))&delim$&str$(gb(8))&delim$&str$(gb(9))&delim$&str$(gb(10))&delim$&ab$(1)&delim$&ab$(2)&delim$&ab$(3)&delim$;
48240     pr #2: str$(finalBillingCode)&delim$;
48260     pr #2: addr$(1)&delim$;
48280     pr #2: addr$(2)&delim$;
48300     pr #2: addr$(3)&delim$;
48320     pr #2: addr$(4)&delim$
48340   loop 
52000 DONE: ! r:
52020   close #h_customer: ioerr ignore
52040   close #2: ioerr ignore
52060   close #3: ioerr ignore
52080   goto XIT ! /r
52100 XIT: ! 
52120   fnxit
54000 ALT_BILL_ADR: ! r:
54020   mat ab$=("")
54040   read #3,using 'Form POS 11,3*C 30',key=z$: mat ab$ nokey ignore
54060 return  ! /r
56000 IGNORE: continue 
58000 ! <Updateable Region: ERTN>
58020 ERTN: fnerror(program$,err,line,act$,"xit")
58040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
58060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
58080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
58100 ERTN_EXEC_ACT: execute act$ : goto ERTN
58120 ! /region
62000 HEADER: ! r:
62020   pr #2: 'Account Key'&delim$;           ! z$
62040   pr #2: 'Meter Address'&delim$;         ! e$(1)
62060   pr #2: 'Name'&delim$;                  ! e$(2)
62080   pr #2: 'Address 1 - Primary'&delim$;   ! e$(3)
62100   pr #2: 'Address 2 - Primary'&delim$;   ! extra$(1)
62120   pr #2: 'CSZ - Primary'&delim$;         ! e$(4)
62140   pr #2: serviceName$(1)&' Meter Number'&delim$; ! f$(1)&delim$;
62160   pr #2: serviceName$(1)&' Rate Code'&delim$; ! str$(a(1))&delim$;
62180   pr #2: serviceName$(2)&' Rate Code'&delim$; ! str$(a(2))&delim$;
62200   pr #2: serviceName$(3)&' Rate Code'&delim$; ! str$(a(3))&delim$;
62220   pr #2: serviceName$(4)&' Rate Code'&delim$; ! str$(a(4))&delim$;
62240   pr #2: serviceName$(5)&' Rate Code'&delim$; ! str$(a(5))&delim$;
62260   pr #2: serviceName$(9)&' Rate Code'&delim$; ! str$(a(6))&delim$;
62280   pr #2: serviceName$(10)&' Rate Code'&delim$; ! str$(a(7))&delim$;
62300   pr #2: serviceName$(1)&' Standard Charge'&delim$; ! str$(b(1))&delim$;
62320   pr #2: serviceName$(2)&' Standard Charge'&delim$; ! str$(b(2))&delim$;
62340   pr #2: serviceName$(3)&' Standard Charge'&delim$; ! str$(b(3))&delim$;
62360   pr #2: serviceName$(4)&' Standard Charge'&delim$; ! str$(b(4))&delim$;
62380   pr #2: serviceName$(5)&' Standard Charge'&delim$; ! str$(b(5))&delim$;
62400   pr #2: serviceName$(6)&' Standard Charge'&delim$; ! str$(b(6))&delim$;
62420   pr #2: serviceName$(7)&' Standard Charge'&delim$; ! str$(b(7))&delim$;
62440   pr #2: serviceName$(1)&' Deposit'&delim$; ! str$(b(8))&delim$;
62460   pr #2: serviceName$(2)&' Deposit'&delim$; ! str$(b(9))&delim$;
62480   pr #2: serviceName$(3)&' Deposit'&delim$; ! str$(b(10))&delim$;
62500   pr #2: serviceName$(4)&' Deposit'&delim$; ! str$(b(11))&delim$;
62520   pr #2: serviceName$(1)&' Deposit Date'&delim$; ! str$(c(1))&delim$;
62540   pr #2: serviceName$(2)&' Deposit Date'&delim$; ! str$(c(2))&delim$;
62560   pr #2: serviceName$(3)&' Deposit Date'&delim$; ! str$(c(3))&delim$;
62580   pr #2: serviceName$(4)&' Deposit Date'&delim$; ! str$(c(4))&delim$;
62600   pr #2: serviceName$(1)&' Reading - Current'&delim$; ! str$(d(1))&delim$;
62620   pr #2: serviceName$(1)&' Reading - Prior'&delim$; ! str$(d(2))&delim$;
62640   pr #2: serviceName$(1)&' Used - Current'&delim$; ! str$(d(3))&delim$;
62660   pr #2: serviceName$(1)&' Used - YTD'&delim$; ! str$(d(4))&delim$;
62680   pr #2: serviceName$(3)&' Reading - Current'&delim$; ! str$(d(5))&delim$;
62700   pr #2: serviceName$(3)&' Reading - Prior'&delim$; ! str$(d(6))&delim$;
62720   pr #2: serviceName$(3)&' KWH Used - Current'&delim$; ! str$(d(7))&delim$;
62740   pr #2: serviceName$(3)&' KWH Used -YTD'&delim$; ! str$(d(8))&delim$;
62760   pr #2: serviceName$(4)&' Reading - Current'&delim$; ! str$(d(9))&delim$;
62780   pr #2: serviceName$(4)&' Reading - Prior'&delim$; ! str$(d(10))&delim$;
62800   pr #2: serviceName$(4)&' Used-Current'&delim$; ! str$(d(11))&delim$;
62820   pr #2: serviceName$(4)&' Used-YTD'&delim$; ! str$(d(12))&delim$;
62840   pr #2: 'Units Per Meter'&delim$; ! str$(d(13))&delim$;
62860   pr #2: 'Demand Multiplier'&delim$; ! str$(d(14))&delim$;
62880   pr #2: 'Demand Reading'&delim$; ! str$(d(15))&delim$;
62900   pr #2: 'Current Balance'&delim$; ! str$(bal)&delim$;
62920   pr #2: 'Date of Charge'&delim$; ! str$(f)&delim$;
62940   pr #2: serviceName$(1)&' Charge'&delim$; ! str$(g(1))&delim$;
62960   pr #2: serviceName$(2)&' Charge'&delim$; ! str$(g(2))&delim$;
62980   pr #2: serviceName$(3)&' Charge'&delim$; ! str$(g(3))&delim$;
63000   pr #2: serviceName$(4)&' Charge'&delim$; ! str$(g(4))&delim$;
63020   pr #2: serviceName$(5)&' Charge'&delim$; ! str$(g(5))&delim$;
63040   pr #2: serviceName$(6)&' Charge'&delim$; ! str$(g(6))&delim$;
63060   pr #2: serviceName$(7)&' Charge'&delim$; ! str$(g(7))&delim$;
63080   pr #2: serviceName$(8)&' Charge'&delim$; ! str$(g(8))&delim$;
63100   pr #2: serviceName$(9)&' Charge'&delim$; ! str$(g(9))&delim$;
63120   pr #2: serviceName$(10)&' Charge'&delim$; ! str$(g(10))&delim$;
63140   pr #2: 'Current Bill – Net Bill'&delim$; ! str$(g(11))&delim$;
63160   pr #2: 'Current Bill – Gross Bill'&delim$; ! str$(g(12))&delim$;
63180   pr #2: 'Alpha Sort Field'&delim$; ! alp$&delim$;
63200   pr #2: serviceName$(3)&' Meter Number'&delim$; ! f$(2)&delim$;
63220   pr #2: serviceName$(4)&' Meter Number'&delim$; ! f$(3)&delim$;
63240   pr #2: 'Alternate Billing Address'&delim$; ! str$(bra)&delim$;
63260   pr #2: serviceName$(1)&' Breakdown'&delim$; ! str$(gb(1))&delim$;
63280   pr #2: serviceName$(2)&' Breakdown'&delim$; ! str$(gb(2))&delim$;
63300   pr #2: serviceName$(3)&' Breakdown'&delim$; ! str$(gb(3))&delim$;
63320   pr #2: serviceName$(4)&' Breakdown'&delim$; ! str$(gb(4))&delim$;
63340   pr #2: serviceName$(5)&' Breakdown'&delim$; ! str$(gb(5))&delim$;
63360   pr #2: serviceName$(6)&' Breakdown'&delim$; ! str$(gb(6))&delim$;
63380   pr #2: serviceName$(7)&' Breakdown'&delim$; ! str$(gb(7))&delim$;
63400   pr #2: serviceName$(8)&' Breakdown'&delim$; ! str$(gb(8))&delim$;
63420   pr #2: serviceName$(9)&' Breakdown'&delim$; ! str$(gb(9))&delim$;
63440   pr #2: serviceName$(10)&' Breakdown'&delim$; ! str$(gb(10))&delim$;
63460   pr #2: 'Name - Alternate Billing'&delim$; ! ab$(1)&delim$;
63480   pr #2: 'Address - Alternate Billing'&delim$; ! ab$(2)&delim$;
63500   pr #2: 'CSZ - Alternate Billing'&delim$; ! ab$(3)
63520   pr #2: 'Final Billing Code'&delim$; ! ab$(3)
63540   pr #2: 'Billing Address 1'&delim$; ! ab$(3)
63560   pr #2: 'Billing Address 2'&delim$; ! ab$(3)
63580   pr #2: 'Billing Address 3'&delim$;! ab$(3)
63600   pr #2: 'Billing Address 4'&delim$ ! ab$(3)
63620 return  ! /r
