10000 ! Replace S:\acsGL\AcPrReg
10200 ! -- PAYROLL REGISTER
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnpedat$,fntos,fnfra,fnlbl,fntxt,fncmdkey,fnacs,fndate_mmddyy_to_ccyymmdd,fngethandle
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim cnam$*40,cap$*128
11400   dim k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
11600   dim fa$(2),sa$(2)*40,adr(2)
11800 ! ______________________________________________________________________
12000   fntop(program$,cap$="Fix Payroll Dates")
12200   fncno(cno,cnam$)
12400   fndat(dat$)
12600 ! ______________________________________________________________________
12800   fntos(sn$="PayrollReg")
13000   rc=cf=0: mylen=22: mypos=mylen+3: frameno=1
13200   fnfra(1,1,3,40,"Date Range for Report","Enter the date range for the payrolls to be included.")
13400   fnlbl(1,1,"Bad Date:",mylen,1,0,frameno)
13600   fntxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno)
13800   resp$(rc+=1)=str$(date_bad)
14000   fnlbl(2,1,"Good Date:",mylen,1,0,frameno)
14200   fntxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this report. ",frameno)
14400   resp$(rc+=1)=str$(date_good)
14600   fncmdkey("Next",1,1,0,"Calculate tax deposit.")
14800   fncmdkey("Cancel",5,0,1,"Returns to menu without printing.")
15000   fnacs(sn$,0,mat resp$,ckey)
15200   if ckey=5 then goto XIT
15400 ! 
15600   date_bad=val(resp$(1))
15800   date_good=val(resp$(2))
16000 ! 
16800   open #h_prmstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
17000   open #h_acprcks:=fngethandle: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&env$('cno')&",Shr",internal,outin,relative 
17400   fnopenprn(cp,58,220,0)
17600   fn_hdr1
17800 L350: if d(1)>0 then goto L360 else goto L390
18000 L360: ! 
18800 L390: read #h_prmstr,using 'Form POS 1,N 4,3*C 25,POS 271,2*N 5': eno,mat k$,mat adr eof FINIS
19000   if adr(1)=0 then goto L390
19200   ca=adr(1)
19400   do 
19600     read #h_acprcks,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca conv L350
19800     if fndate_mmddyy_to_ccyymmdd(d(2))=date_bad then 
20420       d(2)=date(days(date_good,'ccyymmdd'),'mmddyy')
20440       rewrite #h_acprcks,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca
20480       fn_print_1
20500     end if 
20600     if nca=0 then goto L350
20800     ca=nca
21000   loop 
22200   goto FINIS
22400 ! ______________________________________________________________________
22600   def fn_header
22620     nametab=66-int(len(rtrm$(cnam$))/2)
22800     pr #255,using L530: date$,time$,cnam$
23000 L530: form pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
23200     p1=66-int(len(rtrm$(cap$))/2)
23400     pr #255,using L560: rtrm$(cap$)
23600 L560: form pos p1,c 50
24400   fnend 
24600   def fn_hdr1
24800     fn_header
25000     pr #255,using L630: "EMP #    EMPLOYEE NAME         GROSS   FED W/H  FICA W/H    ST W/H  MISC W/H   LOC W/H       NET     TIPS    EIC   DATE    CK #"
25200 L630: form skip 1,c 132,skip 2
25400   fnend 
28800   def fn_print_1
29000     pr #255,using L870: eno,k$(1)(1:19),d(4),d(5),d(6),d(7),d9,d(8),d(22),d(19),d(21),d(2),d(3) pageoflow PGOF1
29400 L870: form pos 1,pic(zzzz),pos 8,c 19,7*n 10.2,pic(------.##),pic(----.##),pic(zzz/zz/zz),pic(zzzzzzz),skip 1
29600   fnend 
29800 PGOF1: ! 
30000   pr #255: newpage
30200   fn_hdr1
30400   continue 
37600 FINIS: ! 
37800   close #h_acprcks: ioerr ignore
38600   fncloseprn
38800   goto XIT
38900 XIT: fnxit
39000 IGNORE: continue 
54800 ! ______________________________________________________________________
55000 ! <updateable region: ertn>
55200 ERTN: fnerror(program$,err,line,act$,"xit")
55400   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
55600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
55800   pr "program pause: type go and press [enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
56000 ERTN_EXEC_ACT: execute act$ : goto ERTN
56200 ! /region
