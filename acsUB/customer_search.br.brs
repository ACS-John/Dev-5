20000 ! replace S:\acsUB\Customer_Search.br
20020 ! search for a customer and return their act number
20040 ! ______________________________________________________________________
20060   def library fncustomer_search(&x$;fixgrid)
20080 ! x$=account   to extract the flexgrid information (master file)
20100     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fnacs,fncmdset,fnerror,fngethandle
20120     on error goto ERTN
20140     dim item$(12)*30,resp$(30)*80,ch$(12),cm$(12)
20160     fntos(sn$="CustomerSrch")
20180     open #file_num:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed ioerr ERTN
20200     restore #file_num: 
20220     mat ch$(12) : mat cm$(12) : mat cm$(12)
20240     ch$(1)="Account"
20260     ch$(2)="Status"
20280     ch$(3)="Name"
20300     ch$(4)="Address"
20320     ch$(5)="Address"
20340     ch$(6)="City, ST Zip"
20360     ch$(7)="Meter Address"
20380     ch$(8)="Route"
20400     ch$(9)="Sequence"
20420     ch$(10)="Phone"
20440     ch$(11)="Meter"
20460     ch$(12)="Alpha"
20480     mat cm$=("80") : cm$(2)="61" : cm$(8)="61": cm$(9)="61"
20500     fnflexinit1('Cust2',1,1,10,72,mat ch$,mat cm$,1)
20520     do 
20530 READ_FILE: ! 
20540       read #file_num,using 'Form POS 1,C 10,pos 1821,c 1,POS 41,C 30,C 30,POS 1864,C 30,POS 101,C 30,POS 11,C 30,POS 1741,C 2,C 7,POS 1894,C 12,POS 131,C 12,pos 354, c 7': mat item$ eof EO_CUSTOMER ioerr ERR_READ
20560       fnflexadd1(mat item$)
20580     loop 
20600 ! ______________________________________________________________________
20620 ERR_READ: ! 
20640     if err<>61 then goto ERTN
20660 ! pr 'Record locked during Customer_Search flexgrid creation - skipped'
20680     read #file_num,release: 
20700     goto READ_FILE
20720 ! ______________________________________________________________________
20740 EO_CUSTOMER: ! 
20760     fncmdset(2)
20780     fnacs(sn$,0,mat resp$,ckey)
20800     let x$=lpad$(resp$(1),10)
20820     if ckey=5 then let x$="          " ! no one selected
20840     goto XIT
20860 ! ______________________________________________________________________
20880 ! <Updateable Region: ERTN>
20900 ERTN: let fnerror(program$,err,line,act$,"xit")
20920     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
20940     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20960     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20980 ERTN_EXEC_ACT: execute act$ : goto ERTN
21000 ! /region
21020 ! ______________________________________________________________________
21040 XIT: close #file_num: ioerr ignore
21060   fnend 
21080 IGNORE: continue 
