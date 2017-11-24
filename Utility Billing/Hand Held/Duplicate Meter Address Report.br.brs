10000 fn_setup
10020 fntop(program$)
12010 activeOnly=1
12020 if u4_meterAddress$='False' then
12040   mat mg$(0)
12060   fnAddOneC(mat mg$,'Meter Address LocationID tracking is currently disabled.')
12080   fnAddOneC(mat mg$,'This report is not applicable.')
12100   fnmsgbox(mat mg$,resp$)
12120   goto Xit
12140 end if
12160 open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
32000 ! r: build list: mat acctList$ (account numbers) and mat LocIdList (LocationIDs)
32020 dim MeterAddress$*30
32040 dim acctList$(0)*10
32060 dim LocIdList(0)
32080 dim meterAddressList$(0)*30
32100 mat acctList$(0)
32120 mat LocIdList(0)
32140 mat meterAddressList$(0)
34000 do
34020   read #hCustomer,using 'form pos 1,C 10,C 30,pos 1821,n 1':  acct$,MeterAddress$,finalBillingCode eof EndBuildList
34040   if ~activeOnly or finalBillingCode=0 then
34060     fnAddOneC(mat acctList$,acct$)
34080     fnAddOneN(mat LocIdList,fnMeterAddressLocationID(MeterAddress$, 1))
34100     fnAddOneC(mat meterAddressList$,meterAddress$)
34120   end if
34140 loop
34160 EndBuildList: ! 
34180 ! /r
42000 ! r: check for duplicate LocationIDs vs Customer records
42020 fnopenprn
42040 for customer=1 to udim(mat acctList$)
42060   searchBefore=srch(mat LocIdList,LocIdList(customer))
42080   if searchBefore=customer then searchBefore=0
42100   searchAfter=srch(mat LocIdList,LocIdList(customer),customer+1)
42120   if searchBefore or searchAfter then
42140     pr #255,using 'form pos 1,C 10,x 1,N 9,x 2,C 30': acctList$(customer),LocIdList(customer),meterAddressList$(customer)
42160   end if
42180 nex customer
42200 fncloseprn
42220 ! /r
50000 close #hCustomer:
50020 Xit: fnxit
62000 def fn_setup
62020   if ~setup then
62040     setup=1
62060     library 'S:\Core\Library': fnAddOneC,fnmsgbox,fnOpenFile,fncreg_read,fncreg_write,fnAddOneN,fntop,fnxit,fnhamster,fngethandle,fnCloseFile,fnerror,fngethandle,fnreg_read,fnreg_write,fnMeterAddressLocationID,fnopenprn,fncloseprn
62120     dim mg$(0)*80
62480   end if
62900 fnend